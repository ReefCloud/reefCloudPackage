# Stage 3 Checkpoint System

## Overview

The checkpoint system allows you to save the complete state of the pipeline after Stage 3 (data processing) completes, so subsequent runs can skip the expensive Steps 1-3 (5-6 hours) and start directly at Step 4 (model fitting).

## Problem Solved

**Before:** Every run takes 5-6 hours for Steps 1-3, then fails in Step 4
**After:** First run saves checkpoint, subsequent runs start at Step 4 in ~5 minutes

---

## What Gets Checkpointed

### Processed Data Files (saved to S3 `/checkpoint/stage3/`):

1. **`reef_data_with_covariates.RData`** (1.3 MB)
   - Benthic observations with Tier2-5 IDs assigned
   - Covariates joined (DHW, cyclones + lag1, lag2)
   - Aggregated by transect, year, depth, benthic group
   - Has PERC_COVER values

2. **`covariates_full_tier5.RData`** (535 KB)
   - Complete Tier5 × Year grid with all covariates
   - sf object with hexagon geometries
   - Used for predictions in modeling

3. **`tiers.sf.RData`** (544 KB)
   - Tier hierarchy with centroids and spatial joins
   - Tier2-5 relationships

4. **`tiers.lookup.RData`**
   - Non-spatial tier hierarchy lookup table

5. **Tier shapefiles** (tier2-5.sf.RData, reef_layer.sf.RData)
   - Spatial polygon boundaries for each tier
   - Coral reef world shapefile

6. **`checkpoint_stage3_manifest.rds`**
   - Metadata: timestamp, version, parameters, file list
   - Used to validate checkpoint integrity

---

## Usage

### First Run (Creates Checkpoint)

```bash
# Run with checkpoint enabled (default)
docker run reefcloud-model-frk:latest \
  --data_path s3://my-bucket/reefcloud/AUS \
  --domain tier \
  --by_tier 5 \
  --model_type type6 \
  --use_checkpoint true
```

**What happens:**
1. Runs Steps 1-3 normally (5-6 hours)
2. After Step 3 completes, automatically uploads checkpoint to S3
3. Proceeds to Step 4

**Log output:**
```
Step 3: Processing data...
[processing logs...]
✓ Processed DHW data: 121195 records

================================================================================
SAVING STAGE 3 CHECKPOINT
================================================================================
  Timestamp: 20251211_143022
  Version: 2025.01.11y

  Uploading processed files to S3:
    ✓ checkpoint_stage3_manifest.rds (0.0 MB)
    ✓ reef_data_with_covariates.RData (1.3 MB)
    ✓ covariates_full_tier5.RData (0.5 MB)
    ✓ tiers.sf.RData (0.5 MB)
    ✓ tiers.lookup.RData (0.2 MB)
    ✓ tier2.sf.RData (0.3 MB)
    ✓ tier3.sf.RData (0.4 MB)
    ✓ tier4.sf.RData (0.6 MB)
    ✓ tier5.sf.RData (1.1 MB)
    ✓ reef_layer.sf.RData (0.8 MB)

  Uploaded 10 of 10 files
  Checkpoint location: s3://my-bucket/reefcloud/AUS/checkpoint/stage3/
================================================================================

Step 4: Fitting models...
```

---

### Subsequent Runs (Uses Checkpoint)

```bash
# Same command, checkpoint automatically detected
docker run reefcloud-model-frk:latest \
  --data_path s3://my-bucket/reefcloud/AUS \
  --domain tier \
  --by_tier 5 \
  --model_type type6 \
  --use_checkpoint true
```

**What happens:**
1. Step 1: Initialize (< 1 min)
2. **Checks S3 for checkpoint** (~5 seconds)
3. **Downloads checkpoint files** (~2-4 minutes)
4. **Skips Steps 2-3 entirely**
5. Proceeds directly to Step 4

**Log output:**
```
Step 1: Initializing...
✓ Path validation completed successfully

================================================================================
CHECKING FOR STAGE 3 CHECKPOINT
================================================================================
  Checking: s3://my-bucket/reefcloud/AUS/checkpoint/stage3/

  Found checkpoint:
    Timestamp: 20251211_143022
    Version: 2025.01.11y
    Files: 10

  Downloading checkpoint files from S3:
    ✓ checkpoint_stage3_manifest.rds (0.0 MB)
    ✓ reef_data_with_covariates.RData (1.3 MB)
    ✓ covariates_full_tier5.RData (0.5 MB)
    ✓ tiers.sf.RData (0.5 MB)
    ✓ tiers.lookup.RData (0.2 MB)
    ✓ tier2.sf.RData (0.3 MB)
    ✓ tier3.sf.RData (0.4 MB)
    ✓ tier4.sf.RData (0.6 MB)
    ✓ tier5.sf.RData (1.1 MB)
    ✓ reef_layer.sf.RData (0.8 MB)

  ✓ Successfully restored checkpoint (10 files)
  Skipping Steps 1-3, proceeding directly to Step 4
================================================================================

==> Checkpoint restored, skipping Steps 2-3

Step 4: Fitting models...
```

**Time saved:** ~5-6 hours → ~5 minutes (60-80x faster startup)

---

### Disable Checkpoint (Force Full Run)

```bash
# Run full pipeline without using checkpoint
docker run reefcloud-model-frk:latest \
  --data_path s3://my-bucket/reefcloud/AUS \
  --domain tier \
  --by_tier 5 \
  --model_type type6 \
  --use_checkpoint false
```

**When to use:**
- Testing changes to data processing code (Steps 2-3)
- Raw data has been updated
- Suspect checkpoint is corrupted
- Want to refresh covariates from GeoServer

---

## Checkpoint Behavior

### Automatic Checkpoint Detection

The system automatically:
1. **Checks for checkpoint** at start of every run (if `--use_checkpoint true`)
2. **Validates checkpoint** (checks manifest, file existence)
3. **Restores checkpoint** if valid
4. **Falls back to full pipeline** if checkpoint missing/invalid

### Checkpoint Invalidation

Checkpoint is **ignored** (full pipeline runs) if:
- Checkpoint files missing from S3
- Manifest corrupted or unreadable
- Any required file fails to download
- `--use_checkpoint false` specified

Checkpoint is **still valid** even if:
- Different version (checkpoint includes version info for reference)
- Different parameters (checkpoint works across parameter changes)

**Recommendation:** Delete checkpoint manually if code changes significantly affect data processing

---

## Manual Checkpoint Management

### View Checkpoint in S3

```bash
aws s3 ls s3://my-bucket/reefcloud/AUS/checkpoint/stage3/ --recursive --human-readable
```

### Delete Checkpoint (Force Refresh)

```bash
aws s3 rm s3://my-bucket/reefcloud/AUS/checkpoint/stage3/ --recursive
```

### Download Checkpoint Locally (Debugging)

```bash
aws s3 cp s3://my-bucket/reefcloud/AUS/checkpoint/stage3/ ./checkpoint/ --recursive
```

---

## Troubleshooting

### Checkpoint Not Found

**Symptom:**
```
No checkpoint found in S3
Will run full pipeline (Steps 1-3)
```

**Causes:**
- First run (checkpoint never created)
- Checkpoint was manually deleted
- Wrong S3 path

**Solution:** Run once with `--use_checkpoint true` to create checkpoint

---

### Checkpoint Restore Failed

**Symptom:**
```
✗ Failed to download: tier5.sf.RData
Checkpoint restore incomplete (9/10 files)
Will run full pipeline
```

**Causes:**
- Network issue during download
- S3 permissions issue
- File corruption in S3

**Solution:**
1. Check AWS credentials and permissions
2. Delete corrupted checkpoint: `aws s3 rm s3://.../checkpoint/stage3/ --recursive`
3. Re-run to create fresh checkpoint

---

### Using Wrong Checkpoint

**Symptom:** Step 4 fails with data mismatch errors

**Cause:** Checkpoint from different data or parameters

**Solution:**
1. Delete old checkpoint
2. Run with `--use_checkpoint false` once to create new checkpoint

---

## Implementation Details

### Files Modified

- **`R/checkpoint_stage3.R`** - New functions: `save_stage3_checkpoint()`, `restore_stage3_checkpoint()`
- **`00_main.R`** - Checkpoint logic added before/after Steps 2-3
- **`R/parseCLA.R`** - Added `USE_CHECKPOINT` parameter parsing
- **`scripts/entrypoint.sh`** - Added `--use_checkpoint` CLI parameter

### Key Functions

**`save_stage3_checkpoint()`**
- Creates manifest with metadata
- Uploads all processed files to S3 `/checkpoint/stage3/`
- Returns TRUE if successful

**`restore_stage3_checkpoint()`**
- Checks S3 for checkpoint existence
- Downloads manifest
- Validates and downloads all checkpoint files
- Returns TRUE if successful, FALSE if checkpoint unavailable

---

## S3 Structure

```
s3://my-bucket/reefcloud/AUS/
├── raw/                          # Input data
├── outputs/                      # Model outputs
├── debug/                        # Debug files
└── checkpoint/
    └── stage3/                   # Stage 3 checkpoint
        ├── checkpoint_stage3_manifest.rds
        ├── processed/
        │   ├── reef_data_with_covariates.RData
        │   ├── covariates_full_tier5.RData
        │   └── tiers.sf.RData
        └── primary/
            ├── tiers.lookup.RData
            ├── tier2.sf.RData
            ├── tier3.sf.RData
            ├── tier4.sf.RData
            ├── tier5.sf.RData
            └── reef_layer.sf.RData
```

---

## Benefits

✅ **60-80x faster restarts** (5-6 hours → 5 minutes)
✅ **Lower AWS costs** (skip GeoServer downloads, less compute time)
✅ **Faster debugging** (iterate on Step 4 errors quickly)
✅ **Automatic fallback** (runs full pipeline if checkpoint unavailable)
✅ **Version tracking** (manifest includes version info)

---

## Best Practices

1. **Always use checkpoint for troubleshooting** - Speeds up iteration on Step 4 bugs
2. **Delete checkpoint when raw data changes** - Ensure processed data is up-to-date
3. **Delete checkpoint after code changes to Steps 2-3** - Avoid stale data
4. **Keep checkpoint enabled in production** - Faster recovery from failures
5. **Monitor checkpoint size** - Currently ~5 MB, should stay small

---

## Future Enhancements (Optional)

- [ ] Checkpoint versioning (keep multiple checkpoints)
- [ ] Checkpoint per benthic group (HARD CORAL, SOFT CORAL, MACROALGAE)
- [ ] Checkpoint compression (reduce S3 storage)
- [ ] Checkpoint validation (checksums, data integrity)
- [ ] Stage 4 partial checkpointing (save per-tier results)
