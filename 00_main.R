## ReefCloud FRK Model - Main Execution Script
##
## This script runs the full FRK/INLA spatial-temporal modeling pipeline
## Environment variables are set by entrypoint.sh based on command-line parameters

library(status)

## Load package from source if in dev mode (source files available)
if (file.exists("/home/project/DESCRIPTION") && dir.exists("/home/project/R")) {
  message("Loading reefCloudPackage from source files...")
  library(reefCloudPackage)  # Load installed version first for dependencies
  # Override with source versions by loading into package namespace
  r_files <- list.files("/home/project/R", pattern = "\\.[Rr]$", full.names = TRUE)
  pkg_env <- new.env(parent = asNamespace("reefCloudPackage"))
  for (f in r_files) {
    tryCatch(source(f, local = pkg_env), error = function(e) {
      message(sprintf("  Warning: failed to source %s: %s", basename(f), e$message))
    })
  }
  # Copy functions from temporary environment into package namespace
  ns <- asNamespace("reefCloudPackage")
  func_names <- ls(pkg_env, all.names = FALSE)
  override_count <- 0
  new_count <- 0
  for (fname in func_names) {
    if (is.function(pkg_env[[fname]])) {
      tryCatch({
        assignInNamespace(fname, pkg_env[[fname]], ns = "reefCloudPackage")
        override_count <- override_count + 1
      }, error = function(e) {
        # Function doesn't exist in installed package — add it to namespace directly
        tryCatch({
          environment(pkg_env[[fname]]) <- ns
          assign(fname, pkg_env[[fname]], envir = ns)
          new_count <<- new_count + 1
        }, error = function(e2) {
          message(sprintf("  Warning: could not inject %s: %s", fname, e2$message))
        })
      })
    }
  }
  message(sprintf("Dev-mode: %d functions overridden, %d new functions added", override_count, new_count))
} else {
  library(reefCloudPackage)
}

# Print version number at startup
version_file <- system.file("VERSION", package = "reefCloudPackage")
if (file.exists(version_file)) {
  version <- trimws(readLines(version_file, n = 1))
  cat(paste0("\n", strrep("=", 60), "\n"))
  cat(paste0("   ReefCloud FRK Model - VERSION ", version, "\n"))
  cat(paste0(strrep("=", 60), "\n\n"))
} else {
  cat("=== ReefCloud FRK Model Execution ===\n")
}

cat("Starting model pipeline...\n\n")

## Stage 1: Initialize (ALWAYS RUN - sets global variables via parseCLA)
cat("Step 1: Initializing...\n")
reefCloudPackage::startMatter()

## Determine checkpoint start stage (must be after startMatter sets CHECKPOINT_START)
checkpoint_start <- ifelse(exists("CHECKPOINT_START", envir = .GlobalEnv), CHECKPOINT_START, 0)

## Parse checkpoint format to determine if it's hierarchical (e.g., "ch_4.1a") or simple (e.g., "2", "3")
checkpoint_is_hierarchical <- FALSE
checkpoint_stage <- 0
checkpoint_group_id <- NULL

if (is.character(checkpoint_start) && grepl("^ch_", checkpoint_start)) {
  checkpoint_is_hierarchical <- TRUE
  # Extract stage number from format like "ch_4.1a" or "ch_4.0"
  stage_match <- regmatches(checkpoint_start, regexec("^ch_([0-9]+)", checkpoint_start))
  if (length(stage_match[[1]]) >= 2) {
    checkpoint_stage <- as.numeric(stage_match[[1]][2])
    checkpoint_group_id <- checkpoint_start
  }
} else {
  checkpoint_stage <- as.numeric(checkpoint_start)
}

## Restore from checkpoint if requested
if (checkpoint_stage > 0) {
  if (checkpoint_is_hierarchical && checkpoint_stage == 4) {
    # Hierarchical Stage 4 checkpoint (ch_4.1a, ch_4.2b, etc.)
    cat(paste0("\nRestoring from hierarchical checkpoint: ", checkpoint_group_id, "\n"))
    restored <- reefCloudPackage::restore_group_checkpoint(checkpoint_group_id)
    if (!restored) {
      stop(paste0("Failed to restore checkpoint ", checkpoint_group_id, ". Cannot continue."))
    }
  } else {
    # Simple stage checkpoint (ch_2, ch_3, or legacy numeric format)
    cat(paste0("\nRestoring from Stage ", checkpoint_stage, " checkpoint...\n"))
    restored <- reefCloudPackage::restore_stage_checkpoint(checkpoint_stage)
    if (!restored) {
      stop(paste0("Failed to restore Stage ", checkpoint_stage, " checkpoint. Cannot continue."))
    }
  }
}

## Stage 2: Load Data
if (checkpoint_stage < 2) {
  cat("\nStep 2: Loading data...\n")
  reefCloudPackage::model_loadData()

  # Save Stage 2 checkpoint (after data loaded)
  if (checkpoint_stage == 0 && exists("ORIGINAL_DATA_PATH", envir = .GlobalEnv)) {
    result <- reefCloudPackage::save_stage_checkpoint(2)
  }
}

## Stage 3: Process Data
if (checkpoint_stage < 3) {
  cat("\nStep 3: Processing data...\n")
  reefCloudPackage::model_processData()

  # Save Stage 3 checkpoint (after data processed)
  if (checkpoint_stage == 0 && exists("ORIGINAL_DATA_PATH", envir = .GlobalEnv)) {
    result <- reefCloudPackage::save_stage_checkpoint(3)
  }
}

## Stage 4: Fit Models (always run)
cat("\nStep 4: Fitting models...\n")
reefCloudPackage::model_fitModel()

cat("\n=== Model Pipeline Completed Successfully ===\n")
