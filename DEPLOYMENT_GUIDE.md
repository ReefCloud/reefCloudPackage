# ReefCloud FRK Model - Deployment Guide

**Version:** 2025.01.11z
**Last Updated:** December 11, 2025

---

## Quick Start

### From Repository Root (Recommended)

```bash
# Navigate to repository root
cd /home/azivalje/julie/reefcloud-model-FRK

# Deploy CDK stack and run task (with checkpoint)
./infrastructure/scripts/deploy-run.sh

# Deploy and run without checkpoint
./infrastructure/scripts/deploy-run.sh --no-checkpoint

# Deploy and run with custom parameters
./infrastructure/scripts/deploy-run.sh --by_tier 4 --spot
```

---

## Deployment Options

### Option 1: Deploy + Run (One Command)

**Use when:** You want to deploy infrastructure changes and immediately run a task

```bash
cd /home/azivalje/julie/reefcloud-model-FRK
./infrastructure/scripts/deploy-run.sh [OPTIONS]
```

**What happens:**
1. Activates CDK virtual environment
2. Deploys infrastructure changes (if any)
3. Starts Fargate task with specified parameters

---

### Option 2: Deploy Only

**Use when:** You only want to update infrastructure without running a task

```bash
cd /home/azivalje/julie/reefcloud-model-FRK
cd infrastructure/cdk
source .venv/bin/activate
cdk deploy --profile adcdev
```

---

### Option 3: Run Only (No Deploy)

**Use when:** Infrastructure is already deployed, just want to run a task

```bash
cd /home/azivalje/julie/reefcloud-model-FRK
./infrastructure/scripts/run-task.sh [OPTIONS]
```

---

## Parameter Reference

### Common Parameters

| Parameter | Values | Default | Description |
|-----------|--------|---------|-------------|
| `--domain` | tier, site | tier | Processing domain |
| `--by_tier` | 2-5 | 5 | Tier level to model |
| `--model_type` | type5, type6 | type6 | Model algorithm |
| `--debug` | true, false | false | Enable debug logging |
| `--refresh_data` | true, false | true | Re-download data from S3 |
| `--use_checkpoint` | true, false | **true** | Use Stage 3 checkpoint |
| `--spot` | (flag) | FARGATE | Use FARGATE_SPOT (70% cheaper) |

### Convenience Flags (deploy-run.sh only)

| Flag | Equivalent | Description |
|------|------------|-------------|
| `--no-checkpoint` | `--use_checkpoint false` | Disable checkpoint system |

---

## Example Workflows

### 1. First Deployment (Creates Checkpoint)

```bash
cd /home/azivalje/julie/reefcloud-model-FRK
./infrastructure/scripts/deploy-run.sh
```

**Expected behavior:**
- CDK deploys infrastructure (~2-5 minutes if changes)
- Fargate task starts
- Steps 1-3 execute (5-6 hours)
- Checkpoint created in S3 after Step 3
- Step 4 proceeds

**Log output to verify:**
```
SAVING STAGE 3 CHECKPOINT
  Uploaded 10 of 10 files
  Checkpoint location: s3://rc-prod-aims-gov-au-reefcloud-frk/AUS/checkpoint/stage3/
```

---

### 2. Subsequent Deployments (Uses Checkpoint)

```bash
cd /home/azivalje/julie/reefcloud-model-FRK
./infrastructure/scripts/deploy-run.sh
```

**Expected behavior:**
- CDK detects no changes (skips deploy)
- Fargate task starts
- **Checkpoint restored from S3 (2-4 minutes)**
- **Steps 2-3 skipped**
- Step 4 starts immediately

**Log output to verify:**
```
CHECKING FOR STAGE 3 CHECKPOINT
  Found checkpoint: ... Files: 10
  ✓ Successfully restored checkpoint (10 files)
  Skipping Steps 1-3, proceeding directly to Step 4
```

**Time saved: 5-6 hours → 5 minutes (60-80x faster)**

---

### 3. Force Full Pipeline (No Checkpoint)

```bash
cd /home/azivalje/julie/reefcloud-model-FRK
./infrastructure/scripts/deploy-run.sh --no-checkpoint
```

**Expected behavior:**
- Full pipeline runs (Steps 1-3 execute)
- No checkpoint restoration
- Useful for testing or after data updates

---

### 4. Custom Parameters

```bash
# Model Tier 4 instead of Tier 5
./infrastructure/scripts/deploy-run.sh --by_tier 4

# Use SPOT instances (70% cheaper, may be interrupted)
./infrastructure/scripts/deploy-run.sh --spot

# Combine multiple parameters
./infrastructure/scripts/deploy-run.sh --by_tier 4 --spot --debug true

# Full pipeline without checkpoint on Tier 4
./infrastructure/scripts/deploy-run.sh --no-checkpoint --by_tier 4
```

---

## Monitoring Your Deployment

### Check Task Status

After deployment starts, the script outputs monitoring commands:

```bash
# Check if task is running
aws ecs describe-tasks \
  --cluster reefcloud-stats-dev \
  --tasks <TASK_ARN> \
  --region ap-southeast-2 \
  --profile adcdev \
  --query 'tasks[0].lastStatus' \
  --output text
```

### View Logs (Real-time)

```bash
# All logs (all tasks)
aws logs tail /aws/ecs/reefcloud-stats-dev \
  --follow \
  --region ap-southeast-2 \
  --profile adcdev

# Specific task only
aws logs tail /aws/ecs/reefcloud-stats-dev \
  --log-stream-names 'reefcloud/reefcloud-stats-model/<TASK_ID>' \
  --follow \
  --region ap-southeast-2 \
  --profile adcdev
```

### Check for Checkpoint Messages

```bash
# Look for checkpoint restoration
aws logs tail /aws/ecs/reefcloud-stats-dev --follow | \
  grep -A10 "CHECKING FOR STAGE 3 CHECKPOINT"

# Look for checkpoint creation
aws logs tail /aws/ecs/reefcloud-stats-dev --follow | \
  grep -A10 "SAVING STAGE 3 CHECKPOINT"
```

---

## Checkpoint Management

### View Checkpoint in S3

```bash
aws s3 ls s3://rc-prod-aims-gov-au-reefcloud-frk/AUS/checkpoint/stage3/ \
  --recursive --human-readable --profile adcdev
```

### Delete Checkpoint

```bash
# Delete to force fresh processing
aws s3 rm s3://rc-prod-aims-gov-au-reefcloud-frk/AUS/checkpoint/stage3/ \
  --recursive --profile adcdev
```

**Delete checkpoint when:**
- Raw data has been updated
- Testing changes to data processing code (Steps 2-3)
- Checkpoint suspected corrupted
- Want fresh covariate downloads from GeoServer

---

## Troubleshooting

### Script Won't Run from Repo Root

**Symptom:** `cd: no such file or directory` or similar errors

**Solution:** The script now automatically resolves paths. Ensure you're running from repo root:
```bash
cd /home/azivalje/julie/reefcloud-model-FRK
./infrastructure/scripts/deploy-run.sh
```

The script will output:
```
==========================================
ReefCloud FRK Model - Deploy and Run
==========================================
Repository: /home/azivalje/julie/reefcloud-model-FRK
==========================================
```

---

### CDK Deploy Fails

**Common causes:**
1. AWS credentials not configured
2. Virtual environment not found
3. Wrong AWS profile

**Solution:**
```bash
# Check AWS credentials
aws sts get-caller-identity --profile adcdev

# Recreate virtual environment if needed
cd infrastructure/cdk
python3 -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
```

---

### Task Fails to Start

**Common causes:**
1. ECR image doesn't exist or wrong tag
2. IAM role permissions incorrect
3. VPC/subnet configuration issue

**Check:**
```bash
# Verify ECR image exists
aws ecr describe-images \
  --repository-name reefcloud-model-frk \
  --image-ids imageTag=2025.01.11z \
  --region ap-southeast-2 \
  --profile adcdev

# Check stack outputs
aws cloudformation describe-stacks \
  --stack-name ReefCloudStatsFargateStack-dev \
  --region ap-southeast-2 \
  --profile adcdev
```

---

### Checkpoint Not Working

**Symptom:** Steps 1-3 always run even after first deployment

**Check:**
1. Verify checkpoint exists in S3
2. Check logs for checkpoint messages
3. Verify `--use_checkpoint true` (default)

```bash
# Check S3
aws s3 ls s3://rc-prod-aims-gov-au-reefcloud-frk/AUS/checkpoint/stage3/ \
  --profile adcdev

# If missing, first run might still be in progress
# Wait for first run to complete Step 3
```

---

## Cost Optimization

### Use Checkpoint + Spot Instances

For maximum cost savings during development:

```bash
./infrastructure/scripts/deploy-run.sh --spot
```

**Benefits:**
- **Checkpoint:** 60-80x faster (5-6 hours → 5 min)
- **FARGATE_SPOT:** 70% cheaper (may be interrupted)
- **Combined:** Massive cost reduction

**Recommended for:**
- Development and testing
- Iterative debugging
- Non-critical analysis

**Not recommended for:**
- Production runs (use regular FARGATE)
- Critical analysis requiring completion guarantee

---

## Pre-Deployment Checklist

Before running `deploy-run.sh`:

- [ ] AWS credentials configured (`aws configure --profile adcdev`)
- [ ] Docker image built and pushed to ECR (version 2025.01.11z)
- [ ] CDK virtual environment exists (`infrastructure/cdk/.venv/`)
- [ ] Repository at latest version (`git pull`)
- [ ] Running from repository root
- [ ] Understand parameter defaults (tier 5, type6, checkpoint enabled)

---

## Post-Deployment Verification

After successful deployment:

- [ ] Task ARN received in output
- [ ] Task status is RUNNING
- [ ] Logs streaming to CloudWatch
- [ ] Checkpoint message appears in logs (first run: save, subsequent: restore)
- [ ] Step 4 proceeds without errors

---

## Next Steps After This Deployment

1. **Monitor first run** (~5-6 hours)
   - Verify checkpoint created successfully
   - Check logs for any errors

2. **Test second run**
   - Run `deploy-run.sh` again
   - Verify checkpoint restored in < 5 minutes
   - Verify Step 4 starts immediately

3. **Test parameter changes**
   - Run with `--by_tier 4` to test different tier
   - Verify warning message if parameters differ
   - Verify checkpoint still used

4. **Test without checkpoint**
   - Run with `--no-checkpoint`
   - Verify full pipeline executes

---

## Additional Resources

- **Checkpoint System:** See `CHECKPOINT_SYSTEM.md`
- **Environment Variables:** See `CHECKPOINT_ENV_VARS.md`
- **Version Details:** See `version-2025.01.11z-checkpoint-system.md`
- **Deployment Scripts:** See `deployment-scripts-updated.md`

---

## Quick Reference Commands

```bash
# Standard deployment (with checkpoint)
./infrastructure/scripts/deploy-run.sh

# Full pipeline (no checkpoint)
./infrastructure/scripts/deploy-run.sh --no-checkpoint

# Custom tier with SPOT
./infrastructure/scripts/deploy-run.sh --by_tier 4 --spot

# View logs
aws logs tail /aws/ecs/reefcloud-stats-dev --follow --profile adcdev

# Check checkpoint
aws s3 ls s3://rc-prod-aims-gov-au-reefcloud-frk/AUS/checkpoint/stage3/ \
  --profile adcdev

# Delete checkpoint
aws s3 rm s3://rc-prod-aims-gov-au-reefcloud-frk/AUS/checkpoint/stage3/ \
  --recursive --profile adcdev
```

---

**End of Deployment Guide**
