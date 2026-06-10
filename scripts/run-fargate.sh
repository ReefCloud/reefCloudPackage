#!/usr/bin/env bash
# Run the ReefCloud FRK pipeline on AWS Fargate.
#
# All defaults match the values used in the local docker run that produced the
# reference outputs. The cluster, subnets, security group, task definition,
# bucket and image are the resources created by reefcloud-services-cdk's
# ReefcloudFrkStack in account 255329909679 (env_name=rc-prod).
#
# Usage:
#   ./scripts/run-fargate.sh                 # all defaults
#   ./scripts/run-fargate.sh --by_tier 4 --basis_resolution 2
#   ./scripts/run-fargate.sh --profile power-reefcloud --follow
#
# After launch the script prints the task ARN and optionally tails CloudWatch
# logs (--follow). The Fargate task may run for hours; you can disconnect the
# tail at any time without affecting the task.

set -euo pipefail

# --------------- Pipeline parameters (mirror local-run defaults) ---------------
DATA_PATH="s3://rc-prod-aims-gov-au-reefcloud-frk/AUS"
DOMAIN="tier"
BY_TIER="5"
MODEL_TYPE="type6"
BASIS_RESOLUTION="3"
REFRESH_DATA="true"
CHECKPOINT_START="0"
DEBUG="false"
SINGLE_TIER=""
PRECISION_RIDGE="0"
OMP_THREADS=""  # Set to "1" to force single-threaded BLAS (deterministic TMB)

# --------------- Runtime expectations -----------------------------------------
# With BASIS_RESOLUTION=3, MODEL_TYPE=type6, BY_TIER=5:
#   - 3 groups (HC, SC, MA) fitted sequentially in subprocesses: ~11-12 hours
#   - Post-fitting (scale_up_pred + attribute_changes): ~45-60 min
#   - Total expected runtime: ~13 hours
#
# IMPORTANT: The ECS task definition (reefcloud-frk-model) in reefcloud-services-cdk
# must have:
#   - stopTimeout: 120 (seconds) — max for Fargate; allows in-progress saveRDS to
#     complete if the task receives SIGTERM (manual stop, spot reclamation, etc.)
#   - No execution timeout (ECS standalone tasks run indefinitely by design)
#   - ephemeralStorage: 200 (GiB) — 24 model files (~17 GB) + raw/processed data
#   - cpu: 16384 (16 vCPU), memory: 126976 (124 GB)
# -------------------------------------------------------------------------------

# --------------- AWS environment (all in 255329909679) ----------------
PROFILE="power-reefcloud"
REGION="ap-southeast-2"
CLUSTER="ReefCloudFRK"
TASK_DEFINITION="reefcloud-frk-model"
CONTAINER_NAME="reefcloud-frk-model"
SUBNETS="subnet-0b80a7f4716475e5b,subnet-0d17252ba5d4de404"
SECURITY_GROUP="sg-03b7d35cd12943912"
LOG_GROUP="reefcloud-frk"

FOLLOW="false"

usage() {
  cat <<EOF
Usage: $(basename "$0") [options]

Pipeline parameters (passed as container command):
  --data_path PATH          (default: $DATA_PATH)
  --domain DOMAIN           (default: $DOMAIN)         tier | site
  --by_tier N               (default: $BY_TIER)        2-5
  --model_type TYPE         (default: $MODEL_TYPE)     type5 | type6
  --basis_resolution N      (default: $BASIS_RESOLUTION) FRK basis resolution
  --refresh_data BOOL       (default: $REFRESH_DATA)
  --checkpoint_start STAGE  (default: $CHECKPOINT_START)
  --debug BOOL              (default: $DEBUG)
  --single_tier ID          (default: empty = all tiers)
                            Diagnostic: only fit this focal-tier id, e.g. 1808.
  --precision_ridge N       (default: $PRECISION_RIDGE) Ridge for precision matrix
                             0 = disabled. Recommended: 1e-6
  --omp_threads N           (default: empty = use all CPUs)
                             Set to 1 for deterministic TMB (single-threaded BLAS)

AWS:
  --profile NAME            (default: $PROFILE)
  --region NAME             (default: $REGION)
  --cluster NAME            (default: $CLUSTER)
  --task_definition NAME    (default: $TASK_DEFINITION)
  --subnets ID1,ID2,...     (default: $SUBNETS)
  --security_group ID       (default: $SECURITY_GROUP)

Behaviour:
  --follow                  tail CloudWatch logs after the task starts
  -h | --help               show this help

EOF
}

while [[ $# -gt 0 ]]; do
  case $1 in
    --data_path)         DATA_PATH="$2"; shift 2 ;;
    --domain)            DOMAIN="$2"; shift 2 ;;
    --by_tier)           BY_TIER="$2"; shift 2 ;;
    --model_type)        MODEL_TYPE="$2"; shift 2 ;;
    --basis_resolution)  BASIS_RESOLUTION="$2"; shift 2 ;;
    --refresh_data)      REFRESH_DATA="$2"; shift 2 ;;
    --checkpoint_start)  CHECKPOINT_START="$2"; shift 2 ;;
    --debug)             DEBUG="$2"; shift 2 ;;
    --single_tier)       SINGLE_TIER="$2"; shift 2 ;;
    --precision_ridge)   PRECISION_RIDGE="$2"; shift 2 ;;
    --omp_threads)       OMP_THREADS="$2"; shift 2 ;;
    --profile)           PROFILE="$2"; shift 2 ;;
    --region)            REGION="$2"; shift 2 ;;
    --cluster)           CLUSTER="$2"; shift 2 ;;
    --task_definition)   TASK_DEFINITION="$2"; shift 2 ;;
    --subnets)           SUBNETS="$2"; shift 2 ;;
    --security_group)    SECURITY_GROUP="$2"; shift 2 ;;
    --follow)            FOLLOW="true"; shift ;;
    -h|--help)           usage; exit 0 ;;
    *) echo "Unknown argument: $1" >&2; usage; exit 1 ;;
  esac
done

# --------------- Print configuration ---------------
echo "=========================================="
echo "ReefCloud FRK on Fargate"
echo "=========================================="
echo "Profile:           $PROFILE"
echo "Region:            $REGION"
echo "Cluster:           $CLUSTER"
echo "Task definition:   $TASK_DEFINITION"
echo "Subnets:           $SUBNETS"
echo "Security group:    $SECURITY_GROUP"
echo "------------------------------------------"
echo "Pipeline parameters:"
echo "  data_path:        $DATA_PATH"
echo "  domain:           $DOMAIN"
echo "  by_tier:          $BY_TIER"
echo "  model_type:       $MODEL_TYPE"
echo "  basis_resolution: $BASIS_RESOLUTION"
echo "  refresh_data:     $REFRESH_DATA"
echo "  checkpoint_start: $CHECKPOINT_START"
echo "  debug:            $DEBUG"
echo "  single_tier:      ${SINGLE_TIER:-(all)}"
echo "  precision_ridge:  $PRECISION_RIDGE"
echo "  omp_threads:      ${OMP_THREADS:-(all)}"
echo "=========================================="

# Build the container override command. Pieces become a JSON array via jq.
NET_CONFIG="awsvpcConfiguration={subnets=[$SUBNETS],securityGroups=[$SECURITY_GROUP],assignPublicIp=DISABLED}"
CMD_PIECES=(
  "--data_path"         "$DATA_PATH"
  "--domain"            "$DOMAIN"
  "--by_tier"           "$BY_TIER"
  "--model_type"        "$MODEL_TYPE"
  "--basis_resolution"  "$BASIS_RESOLUTION"
  "--refresh_data"      "$REFRESH_DATA"
  "--checkpoint_start"  "$CHECKPOINT_START"
  "--debug"             "$DEBUG"
  "--aws_region"        "$REGION"
)
if [ -n "$SINGLE_TIER" ]; then
  CMD_PIECES+=( "--single_tier" "$SINGLE_TIER" )
fi
if [ "$PRECISION_RIDGE" != "0" ]; then
  CMD_PIECES+=( "--precision_ridge" "$PRECISION_RIDGE" )
fi
COMMAND_JSON=$(printf '%s\n' "${CMD_PIECES[@]}" | jq -R . | jq -s .)

# Build environment overrides (for BLAS thread control)
if [ -n "$OMP_THREADS" ]; then
  ENV_JSON='[
    {"name": "OMP_NUM_THREADS", "value": "'"$OMP_THREADS"'"},
    {"name": "OPENBLAS_NUM_THREADS", "value": "'"$OMP_THREADS"'"},
    {"name": "MKL_NUM_THREADS", "value": "'"$OMP_THREADS"'"},
    {"name": "GOTO_NUM_THREADS", "value": "'"$OMP_THREADS"'"}
  ]'
  OVERRIDES=$(jq -n \
    --arg name "$CONTAINER_NAME" \
    --argjson cmd "$COMMAND_JSON" \
    --argjson env "$ENV_JSON" \
    '{containerOverrides: [{name: $name, command: $cmd, environment: $env}]}')
else
  OVERRIDES=$(jq -n \
    --arg name "$CONTAINER_NAME" \
    --argjson cmd "$COMMAND_JSON" \
    '{containerOverrides: [{name: $name, command: $cmd}]}')
fi

echo "Starting Fargate task..."
TASK_ARN=$(aws ecs run-task \
  --cluster "$CLUSTER" \
  --task-definition "$TASK_DEFINITION" \
  --launch-type FARGATE \
  --enable-execute-command \
  --region "$REGION" \
  --profile "$PROFILE" \
  --network-configuration "$NET_CONFIG" \
  --overrides "$OVERRIDES" \
  --query 'tasks[0].taskArn' \
  --output text)

if [[ -z "$TASK_ARN" || "$TASK_ARN" == "None" ]]; then
  echo "ERROR: Failed to start task" >&2
  exit 1
fi

TASK_ID="${TASK_ARN##*/}"

echo
echo "Task started: $TASK_ARN"
echo
echo "Status:"
echo "  aws ecs describe-tasks --cluster $CLUSTER --tasks $TASK_ID --region $REGION --profile $PROFILE \\"
echo "    --query 'tasks[0].[lastStatus,desiredStatus,stoppedReason]' --output text"
echo
echo "Logs (live):"
echo "  aws logs tail $LOG_GROUP --follow --region $REGION --profile $PROFILE \\"
echo "    --log-stream-name-prefix reefcloud-frk/$CONTAINER_NAME/$TASK_ID"
echo
echo "Stop:"
echo "  aws ecs stop-task --cluster $CLUSTER --task $TASK_ID --reason 'manual cancel' \\"
echo "    --region $REGION --profile $PROFILE"
echo

if [[ "$FOLLOW" == "true" ]]; then
  echo "Waiting for task to reach RUNNING state before tailing logs..."
  aws ecs wait tasks-running --cluster "$CLUSTER" --tasks "$TASK_ID" --region "$REGION" --profile "$PROFILE"
  echo "Tailing logs (Ctrl-C to detach; task continues running):"
  aws logs tail "$LOG_GROUP" --follow --region "$REGION" --profile "$PROFILE" \
    --log-stream-name-prefix "reefcloud-frk/$CONTAINER_NAME/$TASK_ID"
fi
