#!/bin/bash
set -e

# ReefCloud FRK Model Entrypoint
# Handles parameter parsing, data copying, model execution, and result export

# Default values
AWS_REGION="ap-southeast-2"
DATA_PATH="s3://rc-prod-aims-gov-au-reefcloud-frk/AUS"
DOMAIN="tier"
BY_TIER="5"
DEBUG="false"
REFRESH_DATA="true"
CHECKPOINT_START="0"
MODEL_TYPE="type6"
BASIS_RESOLUTION="3"
SINGLE_TIER=""
PRECISION_RIDGE="0"
AWS_ACCESS_KEY_ID=""
AWS_SECRET_ACCESS_KEY=""

# Parse command line arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --aws_region)
      AWS_REGION="$2"
      shift 2
      ;;
    --data_path)
      DATA_PATH="$2"
      shift 2
      ;;
    --domain)
      DOMAIN="$2"
      shift 2
      ;;
    --by_tier)
      BY_TIER="$2"
      shift 2
      ;;
    --debug)
      DEBUG="$2"
      shift 2
      ;;
    --refresh_data)
      REFRESH_DATA="$2"
      shift 2
      ;;
    --checkpoint_start)
      CHECKPOINT_START="$2"
      shift 2
      ;;
    --model_type)
      MODEL_TYPE="$2"
      shift 2
      ;;
    --basis_resolution)
      BASIS_RESOLUTION="$2"
      shift 2
      ;;
    --single_tier)
      SINGLE_TIER="$2"
      shift 2
      ;;
    --precision_ridge)
      PRECISION_RIDGE="$2"
      shift 2
      ;;
    --key_id)
      AWS_ACCESS_KEY_ID="$2"
      shift 2
      ;;
    --secret_key)
      AWS_SECRET_ACCESS_KEY="$2"
      shift 2
      ;;
    --help)
      echo "ReefCloud FRK Model - Usage:"
      echo ""
      echo "Parameters:"
      echo "  --aws_region         AWS region (default: ap-southeast-2)"
      echo "  --data_path          Data location (default: s3://rc-prod-aims-gov-au-reefcloud-frk/AUS)"
      echo "  --domain             Processing domain: tier or site (default: tier)"
      echo "  --by_tier            Tier level: 2-5 (default: 5)"
      echo "  --debug              Debug mode: true or false (default: false)"
      echo "  --refresh_data       Refresh data: true or false (default: true)"
      echo "  --checkpoint_start   Checkpoint to restore from (default: 0)"
      echo "                       0 = Full run, save checkpoints after each stage"
      echo "                       2 = Restore stage 2 (ch_2), skip data loading"
      echo "                       3 = Restore stage 3 (ch_3), skip data processing"
      echo "                       ch_4.0 = Restore after data loaded for modeling"
      echo "                       ch_4.1a = Restore HARD CORAL type5 (data-rich tiers)"
      echo "                       ch_4.1b = Restore HARD CORAL type6 (data-sparse tiers)"
      echo "                       ch_4.2a = Restore SOFT CORAL type5"
      echo "                       ch_4.2b = Restore SOFT CORAL type6"
      echo "                       ch_4.3a = Restore MACROALGAE type5"
      echo "                       ch_4.3b = Restore MACROALGAE type6"
      echo "                       ch_4.4 = Restore after predictions scaled up"
      echo "                       ch_4.5 = Restore after attribution analysis"
      echo "  --model_type         Model type: type5, type6 (default: type6)"
      echo "  --basis_resolution   FRK basis resolution: 2 or 3 (default: 3)"
      echo "                       2 = Faster, for large grids. 3 = More detailed, for small datasets"
      echo "  --single_tier        Diagnostic: limit the model loop to a single focal-tier id"
      echo "                       (e.g. 1808). Empty = all tiers (default)."
      echo "  --precision_ridge    Ridge regularization for FRK precision matrix (default: 0)"
      echo "                       0 = disabled (original behavior). Recommended: 1e-6"
      echo "  --key_id             AWS Access Key ID (optional if using IAM role)"
      echo "  --secret_key         AWS Secret Access Key (optional if using IAM role)"
      echo ""
      echo "Data structure requirements:"
      echo "  [data_path]/raw/              - Input data (CSV files)"
      echo "  [data_path]/outputs/tier/     - Tier-level outputs"
      echo "  [data_path]/outputs/site/     - Site-level outputs"
      echo ""
      echo "Example:"
      echo "  docker run reefcloud-model-frk \\"
      echo "    --data_path s3://my-bucket/reefcloud-data \\"
      echo "    --domain tier \\"
      echo "    --by_tier 5 \\"
      echo "    --model_type type6"
      exit 0
      ;;
    *)
      echo "Unknown parameter: $1"
      echo "Use --help for usage information"
      exit 1
      ;;
  esac
done

# DATA_PATH now has a default, no validation needed

# Remove trailing slash from DATA_PATH to avoid double slashes
DATA_PATH="${DATA_PATH%/}"

# Save original DATA_PATH before overwriting it for R process
ORIGINAL_DATA_PATH="$DATA_PATH"

# Display image version for verification
IMAGE_VERSION="unknown"
if [ -f /home/project/VERSION ]; then
  IMAGE_VERSION=$(cat /home/project/VERSION)
fi

echo "=== ReefCloud FRK Model Starting ==="
echo "Image Version: $IMAGE_VERSION"
echo "Configuration:"
echo "  AWS Region: $AWS_REGION"
echo "  Data Path: $ORIGINAL_DATA_PATH"
echo "  Domain: $DOMAIN"
echo "  By Tier: $BY_TIER"
echo "  Debug: $DEBUG"
echo "  Refresh Data: $REFRESH_DATA"
echo "  Checkpoint Start: $CHECKPOINT_START"
echo "  Model Type: $MODEL_TYPE"
echo "  Basis Resolution: $BASIS_RESOLUTION"
echo ""

# Function to log resource usage
log_resources() {
  echo "=== Resource Usage at $(date '+%Y-%m-%d %H:%M:%S') ==="
  # Disk usage
  echo "Disk Usage:"
  df -h /data | tail -n +2 | awk '{print "  Used: " $3 " / " $2 " (" $5 " full)"}'
  # Memory usage
  echo "Memory Usage:"
  free -h | grep "Mem:" | awk '{print "  Used: " $3 " / " $2}'
  # Top processes by memory
  echo "Top 5 Memory Consumers:"
  ps aux --sort=-%mem | head -n 6 | tail -n 5 | awk '{printf "  %s: %s MB\n", $11, $6/1024}'
  echo ""
}

# Log initial resources
log_resources

# Configure AWS credentials if provided
if [ -n "$AWS_ACCESS_KEY_ID" ] && [ -n "$AWS_SECRET_ACCESS_KEY" ]; then
  export AWS_ACCESS_KEY_ID
  export AWS_SECRET_ACCESS_KEY
  export AWS_DEFAULT_REGION="$AWS_REGION"
  echo "✓ AWS credentials configured"
fi

# Detect if DATA_PATH is S3 or local
if [[ "$DATA_PATH" == s3://* ]]; then
  IS_S3=true
  echo "✓ Detected S3 data path"
  # Verify AWS CLI is available
  echo "AWS CLI location: $(which aws 2>&1 || echo 'not found in PATH')"
  echo "AWS CLI version: $(aws --version 2>&1 || echo 'command failed')"
  echo "PATH: $PATH"
else
  IS_S3=false
  echo "✓ Detected local data path"
fi

# Always upload /data/log/ to S3 on script exit so we can debug failures
# even when the model doesn't produce outputs and the run aborts before
# the normal Step 4 sync block.
upload_logs_on_exit() {
  local exit_code=$?
  if [ "$IS_S3" = true ] && [ -d /data/log ] && [ -n "$ORIGINAL_DATA_PATH" ]; then
    echo ""
    echo "=== Uploading /data/log/ to ${ORIGINAL_DATA_PATH}/log/ ==="
    aws s3 sync /data/log/ "${ORIGINAL_DATA_PATH}/log/" --region "$AWS_REGION" --only-show-errors 2>/dev/null || true
  fi
  return $exit_code
}
trap upload_logs_on_exit EXIT

# Step 1: Copy input data to /data/raw
echo ""
echo "=== Step 1: Copying Input Data ==="
if [ "$IS_S3" = true ]; then
  echo "Copying from S3: ${ORIGINAL_DATA_PATH}/raw/ → /data/raw/"
  aws s3 sync "${ORIGINAL_DATA_PATH}/raw/" /data/raw/ --region "$AWS_REGION" --only-show-errors
  echo "✓ Input data copy completed"
else
  echo "Copying from local: ${ORIGINAL_DATA_PATH}/raw/ → /data/raw/"
  cp -r "${ORIGINAL_DATA_PATH}/raw/"* /data/raw/ 2>/dev/null || echo "Warning: No files in ${ORIGINAL_DATA_PATH}/raw/"
fi

# Verify input data
RAW_FILE_COUNT=$(find /data/raw -type f | wc -l)
echo "✓ Found $RAW_FILE_COUNT input files in /data/raw/"

if [ "$RAW_FILE_COUNT" -eq 0 ]; then
  echo "ERROR: No input data files found in ${ORIGINAL_DATA_PATH}/raw/"
  exit 1
fi

# Restore cached covariate data from S3 (avoids re-downloading from GeoServer)
if [ "$IS_S3" = true ]; then
  echo ""
  echo "=== Step 1b: Restoring Cached Covariates ==="
  echo "Syncing from S3: ${ORIGINAL_DATA_PATH}/primary/ → /data/primary/"
  mkdir -p /data/primary
  aws s3 sync "${ORIGINAL_DATA_PATH}/primary/" /data/primary/ --region "$AWS_REGION" --only-show-errors 2>/dev/null || true
  CACHE_COUNT=$(find /data/primary -name "cache_*.RData" -o -name "covariate_*.RData" 2>/dev/null | wc -l)
  if [ "$CACHE_COUNT" -gt 0 ]; then
    echo "✓ Restored $CACHE_COUNT cached covariate files (GeoServer download will be skipped)"
  else
    echo "ℹ No cached covariates found — GeoServer download will run on first use"
  fi
fi

# Verify package extdata files
echo ""
echo "Checking reefCloudPackage extdata files..."
EXTDATA_PATH=$(Rscript -e "cat(system.file('extdata', package='reefCloudPackage'))" 2>/dev/null)
if [ -n "$EXTDATA_PATH" ] && [ -d "$EXTDATA_PATH" ]; then
  EXTDATA_FILE_COUNT=$(find "$EXTDATA_PATH" -type f | wc -l)
  echo "✓ Found $EXTDATA_FILE_COUNT file(s) in package extdata/"
  if [ "$EXTDATA_FILE_COUNT" -gt 0 ]; then
    echo "  Files: $(ls -1 "$EXTDATA_PATH" | tr '\n' ' ')"
  else
    echo "  WARNING: No files found in extdata/ - coral reef shapefile will need to exist in /data/primary/"
  fi
else
  echo "  WARNING: extdata directory not found in package - coral reef shapefile will need to exist in /data/primary/"
fi

# Verify checkpoint functions exist in installed package
echo ""
echo "Checking checkpoint system in installed package..."
Rscript -e "
  has_save <- exists('save_stage_checkpoint', where = asNamespace('reefCloudPackage'))
  has_restore <- exists('restore_stage_checkpoint', where = asNamespace('reefCloudPackage'))
  cat('  save_stage_checkpoint exists:', has_save, '\n')
  cat('  restore_stage_checkpoint exists:', has_restore, '\n')
  if (has_save && has_restore) {
    cat('✓ Checkpoint functions found in package\n')
  } else {
    cat('✗ WARNING: Checkpoint functions NOT found in package\n')
  }

  # List all R files in the package
  pkg_path <- find.package('reefCloudPackage')
  r_files <- list.files(file.path(pkg_path, 'R'), pattern='[.]R$', full.names=FALSE)
  cat('  R files in package:', length(r_files), '\n')
  if (any(grepl('checkpoint', r_files))) {
    cat('  Checkpoint file found:', r_files[grepl('checkpoint', r_files)], '\n')
  } else {
    cat('  No checkpoint file found in R/ directory\n')
  }
"

# Step 2: Set environment variables for reefCloudPackage
echo ""
echo "=== Step 2: Configuring Environment ==="
export DATA_PATH="/data"
export AWS_OUTPUT_PATH="/data/outputs/${DOMAIN}/"
export ORIGINAL_DATA_PATH="$ORIGINAL_DATA_PATH"
export BY_TIER="$BY_TIER"
export DOMAIN="$DOMAIN"
export DEBUG="$DEBUG"
export REFRESH_DATA="$REFRESH_DATA"
export CHECKPOINT_START="$CHECKPOINT_START"
export SINGLE_TIER="$SINGLE_TIER"
export MODEL_TYPE="$MODEL_TYPE"
export BASIS_RESOLUTION="$BASIS_RESOLUTION"
export PRECISION_RIDGE="$PRECISION_RIDGE"

echo "Environment variables set:"
echo "  DATA_PATH=$DATA_PATH"
echo "  ORIGINAL_DATA_PATH=$ORIGINAL_DATA_PATH"
echo "  AWS_OUTPUT_PATH=$AWS_OUTPUT_PATH"
echo "  BY_TIER=$BY_TIER"
echo "  DOMAIN=$DOMAIN"
echo "  MODEL_TYPE=$MODEL_TYPE"
echo "  BASIS_RESOLUTION=$BASIS_RESOLUTION"

# Create output directory
mkdir -p "$AWS_OUTPUT_PATH"

# Step 3: Run the model
echo ""
echo "=== Step 3: Running FRK/INLA Model ==="
echo "Start time: $(date)"
echo ""

cd /home/project

# Run the R script with command-line arguments
# parseCLA() expects: --bucket, --domain, --by_tier, --debug, --refresh_data,
#   --checkpoint_start, --model_type, --single_tier (optional, debug)
RSCRIPT_ARGS=(
  --bucket="$DATA_PATH"
  --domain="$DOMAIN"
  --by_tier="$BY_TIER"
  --debug="$DEBUG"
  --refresh_data="$REFRESH_DATA"
  --checkpoint_start="$CHECKPOINT_START"
  --model_type="$MODEL_TYPE"
)
if [ -n "${SINGLE_TIER:-}" ]; then
  RSCRIPT_ARGS+=( --single_tier="$SINGLE_TIER" )
fi
if [ "${PRECISION_RIDGE:-0}" != "0" ]; then
  RSCRIPT_ARGS+=( --precision_ridge="$PRECISION_RIDGE" )
fi
Rscript 00_main.R "${RSCRIPT_ARGS[@]}"

EXIT_CODE=$?

echo ""
echo "End time: $(date)"

# Log final resource usage
log_resources

if [ $EXIT_CODE -ne 0 ]; then
  echo "ERROR: Model execution failed with exit code $EXIT_CODE"
  exit $EXIT_CODE
fi

# Validate that model outputs were actually created
MODEL_OUTPUT_COUNT=$(find /data/modelled -name "*.RData" -o -name "*.rds" 2>/dev/null | wc -l)
if [ "$MODEL_OUTPUT_COUNT" -eq 0 ]; then
  echo "ERROR: Model execution reported success but no model outputs (.RData or .rds files) were created"
  echo "Check /data/log/reef_data.log for model fitting errors"
  exit 1
fi

echo "✓ Model execution completed successfully"
echo "✓ Model outputs found: $MODEL_OUTPUT_COUNT files"

# Step 4: Copy results back to source location
echo ""
echo "=== Step 4: Exporting Results ==="

OUTPUT_FILE_COUNT=$(find /data/outputs -type f | wc -l)
echo "Found $OUTPUT_FILE_COUNT output files"

if [ "$IS_S3" = true ]; then
  echo "Copying results to S3: /data/outputs/${DOMAIN}/ → ${ORIGINAL_DATA_PATH}/outputs/${DOMAIN}/"
  aws s3 sync "/data/outputs/${DOMAIN}/" "${ORIGINAL_DATA_PATH}/outputs/${DOMAIN}/" --region "$AWS_REGION" --only-show-errors
  echo "✓ Results copy completed"

  # Also sync other data directories (primary, processed, modelled)
  echo "Copying processed data to S3..."
  aws s3 sync /data/primary/ "${ORIGINAL_DATA_PATH}/primary/" --region "$AWS_REGION" --only-show-errors || true
  echo "✓ Primary data copy completed"
  aws s3 sync /data/processed/ "${ORIGINAL_DATA_PATH}/processed/" --region "$AWS_REGION" --only-show-errors || true
  echo "✓ Processed data copy completed"
  aws s3 sync /data/modelled/ "${ORIGINAL_DATA_PATH}/modelled/" --region "$AWS_REGION" --only-show-errors || true
  echo "✓ Modelled data copy completed"
else
  echo "Copying results to local: /data/outputs/${DOMAIN}/ → ${DATA_PATH}/outputs/${DOMAIN}/"
  mkdir -p "${DATA_PATH}/outputs/${DOMAIN}"
  cp -r /data/outputs/${DOMAIN}/* "${DATA_PATH}/outputs/${DOMAIN}/" 2>/dev/null || echo "Warning: No output files to copy"

  # Also copy other data directories
  echo "Copying processed data to local..."
  mkdir -p "${DATA_PATH}/primary" "${DATA_PATH}/processed" "${DATA_PATH}/modelled"
  cp -r /data/primary/* "${DATA_PATH}/primary/" 2>/dev/null || true
  cp -r /data/processed/* "${DATA_PATH}/processed/" 2>/dev/null || true
  cp -r /data/modelled/* "${DATA_PATH}/modelled/" 2>/dev/null || true
fi

echo "✓ Results exported successfully"

# Summary
echo ""
echo "=== Model Execution Summary ==="
echo "✓ Input files processed: $RAW_FILE_COUNT"
echo "✓ Output files generated: $OUTPUT_FILE_COUNT"
echo "✓ Results location: ${DATA_PATH}/outputs/${DOMAIN}/"
echo "✓ Model type: $MODEL_TYPE"
echo "✓ Tier level: $BY_TIER"
echo ""
echo "=== ReefCloud FRK Model Completed Successfully ==="
