# Checkpoint Environment Variables

## Overview

The Stage 3 checkpoint system now saves and validates **environment variables** to ensure consistency between checkpoint creation and restoration.

---

## Environment Variables Saved in Checkpoint

The checkpoint manifest (`checkpoint_stage3_manifest.rds`) includes an `environment` section with these variables:

### **Critical Variables (Required for Step 4)**

1. **`BY_TIER`** (e.g., `5`)
   - Determines which tier level to model
   - Used to calculate `FOCAL_TIER` (BY_TIER=5 → FOCAL_TIER="Tier4")

2. **`MODEL_TYPE`** (e.g., `6`)
   - Controls which model algorithm runs
   - Type 6 = Hybrid (Type5 for data-rich tiers, Type6 for sparse tiers)

3. **`DOMAIN_CATEGORY`** (e.g., `"tier"`)
   - Processing domain: "tier" (spatiotemporal FRK) or "site" (hierarchical INLA)

4. **`BASIS_RESOLUTION`** (e.g., `2`)
   - FRK basis function resolution
   - 2 = Faster (large grids), 3 = More detailed (small datasets)

5. **`DATA_PATH`** (always `"/data/"`)
   - Root directory for all data files

6. **`RDATA_FILE`** (e.g., `"reef_data.RData"`)
   - Name of processed benthic data file

7. **`AWS_PATH`** (e.g., `"s3://bucket/reefcloud/AUS/"`)
   - Original S3 data path (used for outputs)

8. **`ORIGINAL_DATA_PATH`** (e.g., `"s3://bucket/reefcloud/AUS"`)
   - S3 path without trailing slash (used for debug uploads)

### **Optional Variables (Preserved for Consistency)**

9. **`DEBUG_MODE`** (`TRUE`/`FALSE`)
   - Controls logging verbosity

10. **`GENERATE_REPORT`** (`TRUE`/`FALSE`)
    - Whether to generate summary report after modeling

---

## How Environment Variables Are Handled

### **At Checkpoint Creation (After Step 3)**

```r
# Environment variables captured from current session
checkpoint_env <- list(
  BY_TIER = 5,
  MODEL_TYPE = 6,
  DOMAIN_CATEGORY = "tier",
  BASIS_RESOLUTION = 2,
  DATA_PATH = "/data/",
  RDATA_FILE = "reef_data.RData",
  AWS_PATH = "s3://bucket/reefcloud/AUS/",
  ORIGINAL_DATA_PATH = "s3://bucket/reefcloud/AUS",
  DEBUG_MODE = TRUE,
  GENERATE_REPORT = FALSE
)

# Saved in manifest
checkpoint_info <- list(
  timestamp = "20251211_143022",
  version = "2025.01.11y",
  environment = checkpoint_env,  # ← Saved here
  parameters = ...,
  files_included = ...
)
```

### **At Checkpoint Restoration (Start of Run)**

**Step 1 (startMatter) runs normally** and sets environment variables from CLI parameters.

**Then checkpoint restoration:**
1. Downloads checkpoint manifest from S3
2. Reads `checkpoint_info$environment`
3. **Compares** checkpoint variables vs current session variables
4. **Warns** if mismatch detected
5. **Uses current session variables** (does NOT overwrite)

**Why not overwrite?**
- User may intentionally want different parameters
- Step 1 already set variables correctly from current CLI parameters
- Checkpoint is data, not configuration authority

---

## Log Output Examples

### **Checkpoint Creation (After Step 3)**

```
================================================================================
SAVING STAGE 3 CHECKPOINT
================================================================================
  Timestamp: 20251211_143022
  Version: 2025.01.11y

  ✓ Created checkpoint manifest

  Uploading processed files to S3:
    ✓ checkpoint_stage3_manifest.rds (0.0 MB)
    ✓ reef_data_with_covariates.RData (1.3 MB)
    ...
```

The manifest now contains:
```r
{
  "timestamp": "20251211_143022",
  "version": "2025.01.11y",
  "environment": {
    "BY_TIER": "5",
    "MODEL_TYPE": 6,
    "DOMAIN_CATEGORY": "tier",
    "BASIS_RESOLUTION": 2,
    ...
  },
  "files_included": [...]
}
```

---

### **Checkpoint Restoration - Parameters Match**

```
================================================================================
CHECKING FOR STAGE 3 CHECKPOINT
================================================================================
  Checking: s3://bucket/reefcloud/AUS/checkpoint/stage3/

  Found checkpoint:
    Timestamp: 20251211_143022
    Version: 2025.01.11y
    Files: 9
    Parameters: domain=tier, by_tier=5, model_type=6

  Downloading checkpoint files from S3:
    ✓ checkpoint_stage3_manifest.rds (0.0 MB)
    ✓ reef_data_with_covariates.RData (1.3 MB)
    ...

  ✓ Successfully restored checkpoint (9 files)
  Skipping Steps 1-3, proceeding directly to Step 4
================================================================================
```

**Result:** Checkpoint used, no warnings

---

### **Checkpoint Restoration - Parameters Differ**

**Scenario:** Checkpoint created with `by_tier=5`, but current run uses `by_tier=4`

```
================================================================================
CHECKING FOR STAGE 3 CHECKPOINT
================================================================================
  Checking: s3://bucket/reefcloud/AUS/checkpoint/stage3/

  Found checkpoint:
    Timestamp: 20251211_143022
    Version: 2025.01.11y
    Files: 9
    Parameters: domain=tier, by_tier=5, model_type=6
    ⚠ WARNING: Current BY_TIER (4) differs from checkpoint (5)
    ℹ Checkpoint was created with different parameters. Using current parameters.

  Downloading checkpoint files from S3:
    ✓ checkpoint_stage3_manifest.rds (0.0 MB)
    ✓ reef_data_with_covariates.RData (1.3 MB)
    ...

  ✓ Successfully restored checkpoint (9 files)
  Skipping Steps 1-3, proceeding directly to Step 4
================================================================================
```

**Result:** Checkpoint still used, but warning displayed

**Why allow this?**
- The processed data (observations, covariates, tier hierarchies) is the same regardless of `by_tier` parameter
- `by_tier` only affects **which tiers are modeled** in Step 4
- Changing `by_tier=5→4` means modeling Tier3 instead of Tier4, but data is identical

---

## When Parameter Mismatch Matters

### **Safe to Ignore Mismatch:**

✅ **`BY_TIER`** - Data is same, just models different tier level
✅ **`DEBUG_MODE`** - Only affects logging verbosity
✅ **`GENERATE_REPORT`** - Only affects post-modeling output

### **Potentially Problematic Mismatch:**

⚠️ **`MODEL_TYPE`** - Different model algorithm
- Checkpoint data works for any model type (type5, type6)
- BUT predictions/outputs will differ
- **Recommendation:** Delete checkpoint if changing model type

⚠️ **`DOMAIN_CATEGORY`** - Different processing domain
- "tier" vs "site" uses completely different code paths
- **Recommendation:** Delete checkpoint if changing domain

⚠️ **`BASIS_RESOLUTION`** - Different FRK resolution
- Data is same, but model fitting behavior differs
- Safe to change, but predictions will differ slightly

---

## Best Practices

### **1. Use Checkpoint with Same Parameters**

Most reliable approach:
```bash
# First run
docker run ... --by_tier 5 --model_type 6

# Subsequent runs (same parameters)
docker run ... --by_tier 5 --model_type 6  # ✅ Ideal
```

### **2. Delete Checkpoint When Changing Critical Parameters**

If changing `model_type` or `domain`:
```bash
# Delete old checkpoint
aws s3 rm s3://bucket/reefcloud/AUS/checkpoint/stage3/ --recursive

# Run with new parameters
docker run ... --model_type 5  # Will create new checkpoint
```

### **3. Ignore Warnings for Non-Critical Parameters**

Safe to proceed if only `by_tier` or `debug` differs:
```bash
# Checkpoint created with by_tier=5
# Run with by_tier=4 → Warning appears but safe to use
docker run ... --by_tier 4  # ⚠️ Warning, but OK
```

---

## Validation Logic

The checkpoint restoration function validates these parameters:

```r
# Check BY_TIER
if (BY_TIER != checkpoint_info$environment$BY_TIER) {
  cat("⚠ WARNING: Current BY_TIER differs from checkpoint\n")
  param_mismatch <- TRUE
}

# Check MODEL_TYPE
if (MODEL_TYPE != checkpoint_info$environment$MODEL_TYPE) {
  cat("⚠ WARNING: Current MODEL_TYPE differs from checkpoint\n")
  param_mismatch <- TRUE
}

# If any mismatch
if (param_mismatch) {
  cat("ℹ Checkpoint was created with different parameters. Using current parameters.\n")
}
```

**Key Point:** Warnings are informational only. Checkpoint is **always used** if found (unless download fails).

---

## Inspecting Checkpoint Manifest

To view checkpoint metadata without restoring:

```bash
# Download manifest
aws s3 cp s3://bucket/reefcloud/AUS/checkpoint/stage3/checkpoint_stage3_manifest.rds ./

# Read in R
manifest <- readRDS("checkpoint_stage3_manifest.rds")

# View environment variables
manifest$environment
# $BY_TIER
# [1] "5"
# $MODEL_TYPE
# [1] 6
# $DOMAIN_CATEGORY
# [1] "tier"
# ...

# View timestamp
manifest$timestamp
# [1] "20251211_143022"

# View version
manifest$version
# [1] "2025.01.11y"
```

---

## Future Enhancements (Optional)

- [ ] **Strict validation mode** - Fail if parameters differ (add `--strict_checkpoint` flag)
- [ ] **Parameter-specific checkpoints** - Separate checkpoints per `model_type`
- [ ] **Automatic checkpoint invalidation** - Delete checkpoint if parameters differ significantly
- [ ] **Version compatibility checks** - Warn if checkpoint version is very old

---

## Summary

✅ **Environment variables ARE saved** in checkpoint manifest
✅ **Variables are validated** when restoring checkpoint
✅ **Warnings displayed** if current parameters differ from checkpoint
✅ **Current parameters always used** (checkpoint vars are informational only)
✅ **Safe to use checkpoint** even with parameter mismatch in most cases
⚠️ **Delete checkpoint** if changing `model_type` or `domain` for best results
