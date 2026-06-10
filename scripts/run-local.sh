#!/usr/bin/env bash
# Run the ReefCloud FRK pipeline locally in Docker with dev-mode overrides.
#
# This script:
#   1. Uses the existing Docker image (no rebuild needed)
#   2. Bind-mounts R/ and DESCRIPTION to activate dev-mode in 00_main.R
#      → Source R files override installed package functions on the fly
#   3. Bind-mounts tmp/docker-data/ as persistent /data/ via a helper volume
#   4. Pre-populated checkpoint files let you skip stages 2+3 with --checkpoint_start 3
#
# First run (checkpoints already downloaded from S3):
#   ./scripts/run-local.sh                              # all defaults, checkpoint_start=3
#
# Full pipeline (no checkpoint skip):
#   ./scripts/run-local.sh --checkpoint_start 0
#
# Different tier:
#   ./scripts/run-local.sh --single_tier 2345
#
# After editing R code, just re-run — dev-mode picks up changes automatically.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(dirname "$SCRIPT_DIR")"

# --------------- Pipeline defaults (fast local debugging) ---------------
DATA_DIR="$REPO_DIR/tmp/docker-data"
MODEL_TYPE="type6"
BASIS_RESOLUTION="2"
DOMAIN="tier"
BY_TIER="5"
SINGLE_TIER="1808"
PRECISION_RIDGE="0"
CHECKPOINT_START="3"
DEBUG="true"
REFRESH_DATA="false"
IMAGE="reefcloud-model-frk:local"

usage() {
  cat <<EOF
Usage: $(basename "$0") [options]

Local Docker runner with dev-mode (R source override) and persistent data.

Pipeline parameters:
  --model_type TYPE         (default: $MODEL_TYPE)     type5 | type6
  --basis_resolution N      (default: $BASIS_RESOLUTION) FRK basis resolution
  --single_tier ID          (default: $SINGLE_TIER)    tier ID, or "" for all
  --precision_ridge N       (default: $PRECISION_RIDGE) 0=off, 1e-6=recommended
  --checkpoint_start STAGE  (default: $CHECKPOINT_START) 0=full, 3=skip to fitting
  --debug BOOL              (default: $DEBUG)
  --refresh_data BOOL       (default: $REFRESH_DATA)
  --by_tier N               (default: $BY_TIER)

Docker:
  --image NAME:TAG          (default: $IMAGE)
  --data_dir PATH           (default: $DATA_DIR)

  -h | --help               show this help

EOF
}

while [[ $# -gt 0 ]]; do
  case $1 in
    --model_type)        MODEL_TYPE="$2"; shift 2 ;;
    --basis_resolution)  BASIS_RESOLUTION="$2"; shift 2 ;;
    --single_tier)       SINGLE_TIER="$2"; shift 2 ;;
    --precision_ridge)   PRECISION_RIDGE="$2"; shift 2 ;;
    --checkpoint_start)  CHECKPOINT_START="$2"; shift 2 ;;
    --debug)             DEBUG="$2"; shift 2 ;;
    --refresh_data)      REFRESH_DATA="$2"; shift 2 ;;
    --by_tier)           BY_TIER="$2"; shift 2 ;;
    --image)             IMAGE="$2"; shift 2 ;;
    --data_dir)          DATA_DIR="$2"; shift 2 ;;
    -h|--help)           usage; exit 0 ;;
    *) echo "Unknown argument: $1" >&2; usage; exit 1 ;;
  esac
done

# --------------- Verify prerequisites ---------------
if ! docker image inspect "$IMAGE" &>/dev/null; then
  echo "ERROR: Docker image '$IMAGE' not found locally."
  echo "Available reefcloud images:"
  docker images --filter "reference=*reefcloud-model*" --format "  {{.Repository}}:{{.Tag}}"
  exit 1
fi

if [ ! -d "$DATA_DIR/raw" ]; then
  echo "ERROR: Data directory '$DATA_DIR/raw' not found."
  echo "Run: mkdir -p $DATA_DIR/{raw,primary,processed,modelled,outputs/tier,outputs/site,log}"
  echo "Then copy raw data or download checkpoints from S3."
  exit 1
fi

# Verify dev-mode source files exist
if [ ! -f "$REPO_DIR/DESCRIPTION" ] || [ ! -d "$REPO_DIR/R" ]; then
  echo "ERROR: DESCRIPTION or R/ directory not found in $REPO_DIR"
  exit 1
fi

# --------------- Print configuration ---------------
echo "=========================================="
echo "ReefCloud FRK - Local Docker Run"
echo "=========================================="
echo "Image:             $IMAGE"
echo "Data dir:          $DATA_DIR"
echo "Dev-mode source:   $REPO_DIR/R/"
echo "------------------------------------------"
echo "Pipeline parameters:"
echo "  model_type:       $MODEL_TYPE"
echo "  basis_resolution: $BASIS_RESOLUTION"
echo "  single_tier:      ${SINGLE_TIER:-(all)}"
echo "  precision_ridge:  $PRECISION_RIDGE"
echo "  checkpoint_start: $CHECKPOINT_START"
echo "  debug:            $DEBUG"
echo "  refresh_data:     $REFRESH_DATA"
echo "  by_tier:          $BY_TIER"
echo "=========================================="
echo ""

# Show checkpoint status
if [ "$CHECKPOINT_START" != "0" ]; then
  echo "Checkpoint files in $DATA_DIR:"
  for subdir in primary processed; do
    count=$(find "$DATA_DIR/$subdir" -name "*.RData" -o -name "*.rds" 2>/dev/null | wc -l)
    echo "  $subdir/: $count file(s)"
  done
  echo ""
fi

# --------------- Build the docker run command ---------------
# Container args are passed to entrypoint.sh
# --data_path /local-data  → entrypoint copies /local-data/raw/ to /data/raw/
#
# Volume mounts:
#   $DATA_DIR        → /data         persistent data (checkpoints, outputs)
#   $DATA_DIR        → /local-data   entrypoint's source path for raw data copy
#   $REPO_DIR/R      → /home/project/R           dev-mode: source overrides
#   $REPO_DIR/DESCRIPTION → /home/project/DESCRIPTION  dev-mode: trigger
#   $REPO_DIR/00_main.R   → /home/project/00_main.R    latest pipeline driver

CMD_ARGS=(
  --data_path /local-data
  --domain "$DOMAIN"
  --by_tier "$BY_TIER"
  --model_type "$MODEL_TYPE"
  --basis_resolution "$BASIS_RESOLUTION"
  --debug "$DEBUG"
  --refresh_data "$REFRESH_DATA"
  --checkpoint_start "$CHECKPOINT_START"
)
if [ -n "$SINGLE_TIER" ]; then
  CMD_ARGS+=( --single_tier "$SINGLE_TIER" )
fi
if [ "$PRECISION_RIDGE" != "0" ]; then
  CMD_ARGS+=( --precision_ridge "$PRECISION_RIDGE" )
fi

set -x
exec docker run --rm \
  -v "$DATA_DIR":/data \
  -v "$DATA_DIR":/local-data \
  -v "$REPO_DIR/R":/home/project/R:ro \
  -v "$REPO_DIR/DESCRIPTION":/home/project/DESCRIPTION:ro \
  -v "$REPO_DIR/00_main.R":/home/project/00_main.R:ro \
  -v "$REPO_DIR/scripts/entrypoint.sh":/home/project/entrypoint.sh:ro \
  "$IMAGE" \
  "${CMD_ARGS[@]}"
