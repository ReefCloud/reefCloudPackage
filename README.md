# ReefCloud FRK Model

Spatial-temporal modelling of coral reef benthic data using FRK (Fixed Rank Kriging) and INLA, run as a containerised batch job on AWS Fargate.

## What's in this repo

This repository contains the **application** that runs the FRK/INLA pipeline:

- `R/` — `reefCloudPackage` source code
- `00_main.R` — pipeline driver
- `scripts/entrypoint.sh` — container entrypoint (data sync + model run + result export)
- `scripts/run-fargate.sh` — host-side helper to launch the container as a Fargate task
- `Dockerfile` — application image definition (FROM the `reefcloud-base-image`)
- `.github/workflows/build-and-push.yml` — CI: builds the image and pushes to ECR on every push to `main`
- `VERSION` — image tag, bumped on every commit (also stamped into the running container)

The supporting AWS infrastructure lives in two other repos:

- **[reefcloud-security](https://bitbucket.org/aimsdatacentre/reefcloud-security)** — IAM roles (`ReefcloudFRKModelDeployRole`, `ReefcloudFRKTaskRole`, `ReefcloudFRKExecutionRole`)
- **[reefcloud-services-cdk](https://bitbucket.org/aimsdatacentre/reefcloud-services-cdk)** — `ReefcloudFrkStack`: ECR repo, S3 bucket, ECS cluster, security group, log group, Fargate task definition

## Account / resource layout (`rc-prod`, account `255329909679`)

| Resource | Name |
|---|---|
| ECR base image | `255329909679.dkr.ecr.ap-southeast-2.amazonaws.com/reefcloud-base-image:2024.11.1` |
| ECR application image | `255329909679.dkr.ecr.ap-southeast-2.amazonaws.com/reefcloud-model-frk:latest` |
| S3 data bucket | `s3://rc-prod-aims-gov-au-reefcloud-frk` (default prefix `AUS/`) |
| ECS cluster | `ReefCloudFRK` |
| Task definition | `reefcloud-frk-model` |
| CloudWatch log group | `reefcloud-frk` |
| VPC | `vpc-077e7838df39c0b98` (`reefcloud-vpc`) |
| Private subnets | `subnet-0b80a7f4716475e5b`, `subnet-0d17252ba5d4de404` |
| Security group | `sg-…` (`ReefCloudFRKFargateSG`) |

## Building the image

Pushes to `main` automatically build and push the image to ECR via GitHub Actions. The workflow:

1. Authenticates to AWS via OIDC as `arn:aws:iam::255329909679:role/ReefcloudFRKModelDeployRole`.
2. Pulls the base image from ECR.
3. Builds the application image, tagged `:VERSION` and `:latest`.
4. Tests `docker run … --help`.
5. Pushes both tags to ECR.

The build runs unconditionally on every push that touches `Dockerfile`, `R/`, `scripts/`, `00_main.R`, `DESCRIPTION`, `NAMESPACE` or `VERSION`. There is no skip-if-tag-exists guard — pushing the same VERSION re-overwrites the tag in ECR.

`VERSION` should be bumped on every commit (`YYYY.MM.DD[a-z]`); see `git log -- VERSION` for examples. The bump is what gives each ECR image a unique, traceable tag.

## Running the pipeline

### On AWS Fargate (production path)

The recommended way is the helper script — it fills in cluster/subnet/SG/profile defaults and exposes the pipeline parameters as flags:

```bash
./scripts/run-fargate.sh                    # full GBR run with all defaults
./scripts/run-fargate.sh --by_tier 4        # smaller-scope run
./scripts/run-fargate.sh --follow           # tail CloudWatch logs after launch
./scripts/run-fargate.sh --help             # full option list
```

Default profile is `power-reefcloud`. Override with `--profile power-reefcloud` etc. as needed.

The script prints the task ARN and the exact follow-up commands for status, logs, and stop. `--follow` auto-tails CloudWatch once the task hits `RUNNING` (~3-5 min after launch, dominated by image pull).

### Pipeline parameters and defaults

These are the defaults used in the local reference run; the helper script uses the same values:

| Parameter | Default | Notes |
|---|---|---|
| `--data_path` | `s3://rc-prod-aims-gov-au-reefcloud-frk/AUS` | bucket-relative `raw/` subprefix is read; `outputs/`, `primary/`, `processed/`, `modelled/` are written |
| `--domain` | `tier` | `tier` or `site` |
| `--by_tier` | `5` | spatial level: 2 (national) → 5 (reef hexagons) |
| `--model_type` | `type6` | `type6` is hybrid FRK/INLA (data-rich uses FRK, sparse uses INLA); `type5` is INLA-only |
| `--basis_resolution` | `3` | FRK basis resolution. `2` is faster on very large grids; `3` is the production default |
| `--refresh_data` | `true` | re-download covariates from geoserver (cached locally for 7 days, see `R/geoserver_cache.R`) |
| `--checkpoint_start` | `0` | full run; non-zero values restart from a saved checkpoint (S3 only — local runs don't checkpoint) |
| `--debug` | `false` | extra logging |

### Data layout in the bucket

```
s3://rc-prod-aims-gov-au-reefcloud-frk/AUS/
├── raw/             # input: reef_data.zip + tier-{2..5}.json + tiers.zip  (seeded once)
├── primary/         # stage 2 outputs (overwritten each run)
├── processed/       # stage 3 outputs
├── modelled/        # FRK_<group>_<focal-tier>_<id>.RData per (benthic group × tier)
└── outputs/
    ├── tier/        # tier-level CSVs (tier{2,3,4,5}.csv with Group column)
    └── site/        # site-level CSVs
```

The container's entrypoint copies S3 → `/data/raw/` on startup, runs the four model stages, and uploads `outputs/`, `primary/`, `processed/`, `modelled/` back to the bucket on completion.

### Running locally (development only)

For development against a local data tree:

```bash
docker run --rm \
  -v /path/to/your/data:/local-data \
  255329909679.dkr.ecr.ap-southeast-2.amazonaws.com/reefcloud-model-frk:latest \
  --data_path /local-data \
  --domain tier --by_tier 5 --model_type type6 --basis_resolution 3
```

The local run path needs a folder with the same `raw/` contents the bucket has. Local runs **do not save checkpoints** (the entrypoint short-circuits S3 writes when `data_path` is non-S3).

## Pipeline stages

1. **Stage 1 — initialise.** Read CLI args, validate paths, set globals.
2. **Stage 2 — load data.** Unzip `reef_data.csv`, import tier-{2..5} GeoJSON, download covariate layers (DHW, cyclone exposure, …) from `geoserver.apps.aims.gov.au`, load the coral-reefs-of-the-world shapefile.
3. **Stage 3 — process data.** Apply spatial domains, join covariates back to benthic data, deduplicate (Tier5 × year), assemble the per-tier modelling frame.
4. **Stage 4 — fit models.** For each benthic group (HARD CORAL, SOFT CORAL, MACROALGAE) and each tier:
   - Use FRK if the tier has enough data (`type5`/`type6` paths)
   - Fall back to INLA (`type6` path) if data-sparse
   - Save posterior draws and predictions
   - Scale predictions up the tier hierarchy (Tier5 → Tier4 → Tier3 → Tier2)
   - Compute disturbance-effect contrasts
5. **Stage 5 — export.** Write tier CSVs (with `Group` column for benthic-group-aware aggregation), site CSVs, info CSVs, coefficient table.

## Outputs

Tier-level CSVs in `outputs/tier/`:

- `tier2.csv`, `tier3.csv`, `tier4.csv`, `tier5.csv` — predictions with `Group` column
- `info_tier{2..5}.csv` — per-tier metadata (area, year-range, n-observations, …)
- `coef_table.csv` — disturbance-effect coefficients on the logit scale (FRK and INLA harmonised)

Model objects in `modelled/`:

- `FRK_<group>_Tier<focal>_<id>.RData` — list with `group`, `form`, `fitting_method`, `pred_sum_sf`, `post_dist_df`, `data.grp.tier`, `M`. One file per (benthic group × focal tier) combination — group name is now in the filename so per-group model artefacts no longer collide.

## Resource sizing

| Stage | Bottleneck | Sizing on Fargate |
|---|---|---|
| 2 (covariates) | network to geoserver | ~30 min total; 7-day local cache makes reruns fast |
| 3 (processing) | CPU + memory for spatial joins | typically <30 min |
| 4 (modelling) | CPU + memory for FRK basis matrices | hours; 16 vCPU + 64 GiB is the minimum CPU/max memory pairing Fargate supports |

The task definition is 16 vCPU / 64 GiB / 200 GiB ephemeral storage.

## Monitoring

Fargate task status:
```bash
aws ecs describe-tasks --cluster ReefCloudFRK --tasks <task-id> \
  --region ap-southeast-2 --profile power-reefcloud \
  --query 'tasks[0].[lastStatus,desiredStatus,stoppedReason]' --output text
```

Live logs (CloudWatch):
```bash
aws logs tail reefcloud-frk --follow \
  --region ap-southeast-2 --profile power-reefcloud \
  --log-stream-name-prefix reefcloud-frk/reefcloud-frk-model/<task-id>
```

`./scripts/run-fargate.sh --follow` does the wait+tail combination automatically.

## Troubleshooting

**No input data found** — the bucket's `raw/` prefix is empty. Sync the input files first:
```bash
aws s3 sync /path/to/local/raw/ s3://rc-prod-aims-gov-au-reefcloud-frk/AUS/raw/ \
  --profile power-reefcloud --region ap-southeast-2
```

**Out-of-memory at FRK fitting** — large tiers with `basis_resolution=3` can exceed the 64 GiB container limit. Drop to `--basis_resolution 2` for that run.

**S3 access denied** — `ReefcloudFRKTaskRole` needs `Get/Put/DeleteObject` on `rc-prod-aims-gov-au-reefcloud-frk`. The role definition is in `reefcloud-security`.

**ECR pull fails on the runner** — the AIMS self-hosted runner must allow egress to `*.dkr.ecr.ap-southeast-2.amazonaws.com` and the corporate firewall must allowlist the new account's registry endpoint.

## Related repositories

- `reefcloud-base-image` — base R image with FRK, INLA, sf, terra, cmdstanr
- `reefcloud-security` — IAM roles
- `reefcloud-services-cdk` — `ReefcloudFrkStack` (and the rest of the AIMS reefcloud AWS infra)

## Version history

See `git log -- VERSION` and `CHANGELOG.md`.

## License

GPL-3

## Support

- Open an issue in this repository
- Contact: reefcloud@aims.gov.au

## Contributors

- Murray Logan
- Julie Vercelloni
- Alex Zivaljevic
- ReefCloud Team
