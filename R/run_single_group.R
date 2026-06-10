#' @title Run Model Fitting for a Single Benthic Group
#' @description Fits type5 and type6 models for one benthic group (MODEL_TYPE=6 path).
#' Extracted from the group loop body of model_fitModelTier() so it can be run
#' in a subprocess for memory isolation.
#' @param group_name Character string: "HARD CORAL", "SOFT CORAL", or "MACROALGAE"
#' @param group_id   Character string: "4.1", "4.2", or "4.3"
#' @return Invisible NULL. Side effects: writes model RDS files to DATA_PATH/modelled/
#'   and uploads group checkpoints to S3.
#' @author Julie Vercelloni, Alex (subprocess extraction)
#' @export
run_single_group <- function(group_name, group_id) {

  # ---- Resolve globals EXPLICITLY from .GlobalEnv ----
  # Package namespace scoping may not reach .GlobalEnv reliably for free variables.
  # status::status_try_catch silently swallows errors and returns error objects,

  # so we validate every result explicitly.
  GROUP <- group_name
  assign("GROUP", GROUP, envir = .GlobalEnv)

  # Explicitly resolve key globals from .GlobalEnv
  if (!exists("GROUPS", envir = .GlobalEnv)) stop("GROUPS not found in .GlobalEnv after load_data_for_model()")
  GROUPS <- get("GROUPS", envir = .GlobalEnv)

  if (!exists("data", envir = .GlobalEnv)) stop("'data' not found in .GlobalEnv after load_data_for_model()")
  data <- get("data", envir = .GlobalEnv)

  if (!exists("tier.sf", envir = .GlobalEnv)) stop("'tier.sf' not found in .GlobalEnv after load_data_for_model()")
  tier.sf <- get("tier.sf", envir = .GlobalEnv)

  BY_TIER <- get("BY_TIER", envir = .GlobalEnv)
  DEBUG_MODE <- get("DEBUG_MODE", envir = .GlobalEnv)

  # ---- Validate data object ----
  cat(sprintf("  [DIAG] data class: %s | nrow: %s | ncol: %s\n",
              paste(class(data), collapse=","), NROW(data), NCOL(data)))
  cat(sprintf("  [DIAG] data columns: %s\n",
              paste(head(names(data), 20), collapse=", ")))

  if (!is.data.frame(data)) {
    stop(sprintf("'data' is not a data.frame (class: %s). load_data_for_model() likely failed silently.",
                 paste(class(data), collapse=",")))
  }
  if (!"fGROUP" %in% names(data)) {
    stop(sprintf("'data' has no 'fGROUP' column. Columns: %s",
                 paste(names(data), collapse=", ")))
  }

  if (!GROUP %in% GROUPS) {
    cat(sprintf("  Group '%s' not in GROUPS, skipping\n", GROUP))
    return(invisible(NULL))
  }

  # Check if GROUP exists in actual data
  groups_in_data <- unique(data$fGROUP)
  cat(sprintf("  [DIAG] Groups in data$fGROUP: %s\n", paste(groups_in_data, collapse=", ")))
  if (!GROUP %in% groups_in_data) {
    cat(sprintf("  Group '%s' not found in data$fGROUP, skipping\n", GROUP))
    return(invisible(NULL))
  }

  if (!DEBUG_MODE) cli::cli_h1(paste0("Modeling ", GROUP))

  # ---- Prepare group data ----
  data.grp <- reefCloudPackage::prep_group_data_for_modelling(data, GROUP)

  # Validate: prep_group_data_for_modelling uses status_try_catch which may
  # return an error object silently instead of a data.frame
  if (!is.data.frame(data.grp)) {
    stop(sprintf(
      "prep_group_data_for_modelling() returned %s instead of data.frame. Error swallowed by status_try_catch. Class: %s",
      ifelse(inherits(data.grp, "error"), paste0("ERROR: ", conditionMessage(data.grp)), "non-df"),
      paste(class(data.grp), collapse=",")
    ))
  }
  cat(sprintf("  [DIAG] data.grp: %d rows, %d cols\n", nrow(data.grp), ncol(data.grp)))
  if (nrow(data.grp) == 0) {
    stop(sprintf("prep_group_data_for_modelling() returned 0 rows for group '%s'", GROUP))
  }

  # ---- MODEL_TYPE == 6: hybrid (type5 + type6) ----
  FOCAL_TIER <- paste0('Tier', as.numeric(BY_TIER) - 1)
  cat(sprintf("  [DIAG] FOCAL_TIER: %s | present in data.grp: %s\n",
              FOCAL_TIER, FOCAL_TIER %in% names(data.grp)))

  if (!FOCAL_TIER %in% names(data.grp)) {
    stop(sprintf("Column '%s' not found in data.grp. Available: %s",
                 FOCAL_TIER, paste(names(data.grp), collapse=", ")))
  }

  # --- Type 5: tiers with enough data (>=3 sites, >=2 years) ---
  data.grp.enough <- reefCloudPackage::filter_focaltier_enough(
    data.grp, FOCAL_TIER, n.spat = 3, n.temp = 2, i = 1, N = 1
  )

  # Validate filter result (status_try_catch may return error object)
  if (!is.data.frame(data.grp.enough)) {
    msg <- if (inherits(data.grp.enough, "error")) conditionMessage(data.grp.enough) else paste(class(data.grp.enough), collapse=",")
    cat(sprintf("  [WARN] filter_focaltier_enough returned non-df: %s\n", msg))
    # Make it an empty data.frame so type5 is skipped but we continue to type6
    data.grp.enough <- data.grp[0, ]
  }
  n_tiers_enough <- length(unique(data.grp.enough[[FOCAL_TIER]]))
  cat(sprintf("  [DIAG] Type5 tiers (enough data): %d (%d rows)\n", n_tiers_enough, nrow(data.grp.enough)))

  # Initialise tier status tracker
  assign("TIER_STATUS", list(), envir = .GlobalEnv)

  if (nrow(data.grp.enough) > 0) {
    reefCloudPackage::model_fitModelTier_type5(data.grp.enough, tier.sf)
  } else {
    cat("  No tiers with enough data for type5, skipping.\n")
  }

  # ---- Save ch_X.Xa checkpoint (type5 complete) ----
  checkpoint_name_a <- paste0("ch_", group_id, "a")
  if (exists("TIER_STATUS", envir = .GlobalEnv) && length(get("TIER_STATUS", envir = .GlobalEnv)) > 0) {
    reefCloudPackage::save_group_checkpoint(
      checkpoint_name = checkpoint_name_a,
      group           = GROUP,
      method          = "type5",
      tiers_status    = get("TIER_STATUS", envir = .GlobalEnv)
    )
    assign("TIER_STATUS", list(), envir = .GlobalEnv)
  }

  # --- Type 6: tiers without enough data (fallback) ---
  data.grp.not.enough <- reefCloudPackage::filter_focaltier_not_enough(
    data.grp, FOCAL_TIER, n.spat = 3, n.temp = 2, i = 1, N = 1
  )

  # Validate filter result
  if (!is.data.frame(data.grp.not.enough)) {
    msg <- if (inherits(data.grp.not.enough, "error")) conditionMessage(data.grp.not.enough) else paste(class(data.grp.not.enough), collapse=",")
    cat(sprintf("  [WARN] filter_focaltier_not_enough returned non-df: %s\n", msg))
    data.grp.not.enough <- data.grp[0, ]
  }
  n_tiers_not_enough <- length(unique(data.grp.not.enough[[FOCAL_TIER]]))
  cat(sprintf("  [DIAG] Type6 tiers (not enough data): %d (%d rows)\n", n_tiers_not_enough, nrow(data.grp.not.enough)))

  if (nrow(data.grp.not.enough) > 0) {
    reefCloudPackage::model_fitModelTier_type6(data.grp.not.enough, tier.sf)
  } else {
    cat("  No tiers for type6 fallback, skipping.\n")
  }

  # ---- Save ch_X.Xb checkpoint (type6 complete) ----
  checkpoint_name_b <- paste0("ch_", group_id, "b")
  if (exists("TIER_STATUS", envir = .GlobalEnv) && length(get("TIER_STATUS", envir = .GlobalEnv)) > 0) {
    reefCloudPackage::save_group_checkpoint(
      checkpoint_name = checkpoint_name_b,
      group           = GROUP,
      method          = "type6",
      tiers_status    = get("TIER_STATUS", envir = .GlobalEnv)
    )
    assign("TIER_STATUS", list(), envir = .GlobalEnv)
  }

  # ---- Final validation: check model files were actually produced ----
  modelled_dir <- paste0(get("DATA_PATH", envir = .GlobalEnv), "modelled/")
  group_pattern <- gsub(" ", ".", GROUP)
  model_files <- list.files(modelled_dir, pattern = group_pattern, full.names = TRUE)
  cat(sprintf("  [DIAG] Model files for %s in %s: %d files\n", GROUP, modelled_dir, length(model_files)))
  if (length(model_files) > 0) {
    cat(sprintf("    Files: %s\n", paste(basename(model_files), collapse=", ")))
  } else {
    stop(sprintf(
      "FATAL: No model output files produced for group '%s'. Type5 tiers: %d, Type6 tiers: %d. Model fitting silently failed.",
      GROUP, n_tiers_enough, n_tiers_not_enough))
  }

  cat(sprintf("\n=== Group %s completed ===\n", GROUP))
  invisible(NULL)
}


#' @title Subprocess Entry Point for Group Fitting
#' @description Sets up the R environment (parseCLA, generateSettings, etc.) from
#' inherited environment variables and then runs \code{run_single_group()}.
#' Intended to be called by \code{Rscript -e} from a forked subprocess so that
#' all memory is reclaimed when the process exits.
#' @param group_name Character string: "HARD CORAL", "SOFT CORAL", or "MACROALGAE"
#' @param group_id   Character string: "4.1", "4.2", or "4.3"
#' @export
run_group_in_subprocess <- function(group_name, group_id) {

  cat(sprintf("\n%s\n", strrep("=", 80)))
  cat(sprintf("SUBPROCESS: Starting group '%s' (id=%s)\n", group_name, group_id))
  cat(sprintf("  PID: %d | Memory: %.0f MB\n", Sys.getpid(),
              as.numeric(gc(reset = TRUE)[2, 2])))
  cat(sprintf("%s\n\n", strrep("=", 80)))

  # ---- Attach packages needed by model functions ----
  # The reefCloudPackage NAMESPACE has no importFrom() directives, so package
  # functions that use %>, sym(), str_detect(), etc. rely on these being on the

  # search path. In the parent process they are attached via library() in
  # 00_main.R/startMatter, but in a fresh subprocess they must be loaded here.
  suppressPackageStartupMessages({
    library(dplyr)       # provides %>% (re-exported from magrittr), sym, filter, mutate, etc.
    library(sf)          # spatial operations used in model fitting
    library(stringr)     # str_detect, str_replace used in filter/model functions
    library(tidyr)       # pivot/nest operations
    library(rlang)       # !!, sym, :=
  })

  # ---- Initialize status system (stage 1 first for parseCLA/generateSettings) ----
  status::status_initialize(pkgs = "reefCloudPackage")
  status::status_set_stage(stage = 1, title = "Settings")
  # Suppress status box display (noisy in CloudWatch)
  tryCatch(
    assignInNamespace("display_status_terminal", function() invisible(NULL), ns = "status"),
    error = function(e) NULL
  )

  # ---- Set up globals from inherited environment variables ----
  # These env vars are set by entrypoint.sh and propagated to the subprocess.
  # parseCLA reads from command-line args, so we construct synthetic args.
  args <- c(
    paste0("--bucket=",      Sys.getenv("DATA_PATH",  "/data")),
    paste0("--domain=",      Sys.getenv("DOMAIN",     "tier")),
    paste0("--by_tier=",     Sys.getenv("BY_TIER",    "5")),
    paste0("--model_type=",  Sys.getenv("MODEL_TYPE", "type6")),
    paste0("--debug=",       Sys.getenv("DEBUG",      "false")),
    "--refresh_data=false",
    "--checkpoint_start=0"
  )
  single_tier <- Sys.getenv("SINGLE_TIER", "")
  if (nzchar(single_tier)) args <- c(args, paste0("--single_tier=", single_tier))

  precision_ridge <- Sys.getenv("PRECISION_RIDGE", "0")
  if (nzchar(precision_ridge) && precision_ridge != "0") {
    args <- c(args, paste0("--precision_ridge=", precision_ridge))
  }

  cat(sprintf("  [DIAG] parseCLA args: %s\n", paste(args, collapse=" ")))
  reefCloudPackage::parseCLA(args)

  # Validate parseCLA succeeded (it's wrapped in status_try_catch which may swallow errors)
  if (!exists("AWS_PATH", envir = .GlobalEnv)) {
    stop("FATAL: parseCLA() failed silently — AWS_PATH not set. status_try_catch likely swallowed error at stage mismatch.")
  }
  cat(sprintf("  [DIAG] After parseCLA: AWS_PATH=%s, BY_TIER=%s, MODEL_TYPE=%s\n",
              get("AWS_PATH", envir = .GlobalEnv),
              get("BY_TIER", envir = .GlobalEnv),
              get("MODEL_TYPE", envir = .GlobalEnv)))

  # Set additional globals from env vars (same logic as startMatter)
  odp <- Sys.getenv("ORIGINAL_DATA_PATH", "")
  if (nzchar(odp)) {
    assign("ORIGINAL_DATA_PATH", odp, envir = .GlobalEnv)
    ORIGINAL_DATA_PATH <<- odp
  }
  br <- Sys.getenv("BASIS_RESOLUTION", "3")
  if (nzchar(br)) {
    assign("BASIS_RESOLUTION", as.integer(br), envir = .GlobalEnv)
    BASIS_RESOLUTION <<- as.integer(br)
  }

  # ---- Generate path settings ----
  reefCloudPackage::generateSettings()

  # Validate generateSettings succeeded
  if (!exists("RDATA_FILE", envir = .GlobalEnv)) {
    stop("FATAL: generateSettings() failed silently — RDATA_FILE not set.")
  }
  cat(sprintf("  [DIAG] After generateSettings: DATA_PATH=%s, RDATA_FILE=%s\n",
              get("DATA_PATH", envir = .GlobalEnv),
              get("RDATA_FILE", envir = .GlobalEnv)))

  # ---- Now switch to stage 4 for model data loading ----
  status::status_set_stage(stage = 4, title = "Model data")

  # ---- Create directory structure (SAFE subprocess version) ----
  # We CANNOT call reefCloudPackage::config() here because:
  #   1. config() references LOG_FILE (line 15) which is only set by
  #      initialise_log() — never called in subprocess -> crash.
  #   2. config() calls unlink(paste0(AWS_OUTPUT_PATH, "*.*")) which
  #      would delete output CSVs from prior groups (destructive).
  # Instead, we do the safe subset: ensure directories exist, set globals.
  DATA_PATH <- get("DATA_PATH", envir = .GlobalEnv)
  for (d in c(DATA_PATH, paste0(DATA_PATH, "log"),
              paste0(DATA_PATH, "primary"), paste0(DATA_PATH, "processed"),
              paste0(DATA_PATH, "modelled"), paste0(DATA_PATH, "summarised"))) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  }
  if (exists("AWS_PATH", envir = .GlobalEnv) && !dir.exists(get("AWS_PATH", envir = .GlobalEnv))) {
    dir.create(get("AWS_PATH", envir = .GlobalEnv), recursive = TRUE)
  }
  if (exists("AWS_OUTPUT_PATH", envir = .GlobalEnv) && !dir.exists(get("AWS_OUTPUT_PATH", envir = .GlobalEnv))) {
    dir.create(get("AWS_OUTPUT_PATH", envir = .GlobalEnv), recursive = TRUE)
  }

  # Set LOG_FILE so downstream functions that reference it don't crash
  FILENAME <- if (exists("FILENAME", envir = .GlobalEnv)) get("FILENAME", envir = .GlobalEnv) else "subprocess"
  LOG_FILE <<- paste0(DATA_PATH, "log/", FILENAME, ".log")
  if (!file.exists(LOG_FILE)) file.create(LOG_FILE)

  # Set OUTPUT_PATH and DOCS_PATH (normally set by config())
  OUTPUT_PATH <<- "../output/"
  if (!dir.exists(OUTPUT_PATH)) dir.create(OUTPUT_PATH, recursive = TRUE)
  if (!dir.exists(paste0(OUTPUT_PATH, "figures"))) dir.create(paste0(OUTPUT_PATH, "figures"), recursive = TRUE)
  if (!dir.exists(paste0(OUTPUT_PATH, "figures/spatial/"))) dir.create(paste0(OUTPUT_PATH, "figures/spatial/"), recursive = TRUE)
  DOCS_PATH <<- "../docs/"

  # ---- Load data from disk (stages 1-3 already wrote these files) ----
  cat("  Loading data for modeling...\n")

  # Validate that generateSettings set RDATA_FILE
  if (!exists("RDATA_FILE", envir = .GlobalEnv)) {
    stop("FATAL: generateSettings() failed silently — RDATA_FILE not set. Check that parseCLA + generateSettings work in subprocess.")
  }
  cat(sprintf("  [DIAG] RDATA_FILE: %s\n", get("RDATA_FILE", envir = .GlobalEnv)))
  cat(sprintf("  [DIAG] DATA_PATH: %s\n", DATA_PATH))

  # Check which file will be loaded
  rdata_cov_file <- stringr::str_replace(get("RDATA_FILE", envir = .GlobalEnv), "\\.RData$", "_with_covariates.RData")
  rdata_cov_path <- file.path(DATA_PATH, "processed", rdata_cov_file)
  rdata_path <- file.path(DATA_PATH, "processed", get("RDATA_FILE", envir = .GlobalEnv))
  cat(sprintf("  [DIAG] Checking covariate file: %s -> exists: %s\n", rdata_cov_path, file.exists(rdata_cov_path)))
  cat(sprintf("  [DIAG] Checking base file: %s -> exists: %s\n", rdata_path, file.exists(rdata_path)))

  reefCloudPackage::load_data_for_model()

  # Validate data was loaded
  if (!exists("data", envir = .GlobalEnv)) {
    # List what WAS loaded
    cat("  [DIAG] .GlobalEnv objects after load_data_for_model():\n")
    cat(sprintf("    %s\n", paste(ls(.GlobalEnv), collapse=", ")))
    stop("FATAL: load_data_for_model() did not create 'data' in .GlobalEnv")
  }
  loaded_data <- get("data", envir = .GlobalEnv)
  cat(sprintf("  [DIAG] 'data' class: %s | nrow: %s | ncol: %s\n",
              paste(class(loaded_data), collapse=","), NROW(loaded_data), NCOL(loaded_data)))
  if (is.data.frame(loaded_data)) {
    cat(sprintf("  [DIAG] 'data' columns (first 15): %s\n",
                paste(head(names(loaded_data), 15), collapse=", ")))
    if ("fGROUP" %in% names(loaded_data)) {
      cat(sprintf("  [DIAG] unique fGROUP values: %s\n",
                  paste(unique(loaded_data$fGROUP), collapse=", ")))
    } else {
      cat("  [WARN] 'data' has no 'fGROUP' column!\n")
    }
  } else {
    cat(sprintf("  [WARN] 'data' is NOT a data.frame! Likely an error object from status_try_catch.\n"))
    if (inherits(loaded_data, "error")) {
      cat(sprintf("  [WARN] Error message: %s\n", conditionMessage(loaded_data)))
    }
  }

  cat(sprintf("  Data loaded. Memory: %.0f MB\n\n", sum(gc()[, 2])))

  # ---- Run the group ----
  reefCloudPackage::run_single_group(group_name, group_id)

  cat(sprintf("\n%s\n", strrep("=", 80)))
  cat(sprintf("SUBPROCESS: Group '%s' finished. Process exiting to free memory.\n", group_name))
  cat(sprintf("  Final memory: %.0f MB\n", sum(gc()[, 2])))
  cat(sprintf("%s\n\n", strrep("=", 80)))
}
