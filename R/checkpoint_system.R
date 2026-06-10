#' @title Save Stage Checkpoint to S3
#' @description Saves all data and environment variables for a given stage
#' @param stage_num Integer (1, 2, or 3)
#' @return Logical indicating success
#' @export
save_stage_checkpoint <- function(stage_num) {

  checkpoint_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  checkpoint_version <- system.file("VERSION", package = "reefCloudPackage") %>%
    readLines(n = 1) %>%
    trimws()

  cat("\n")
  cat(strrep("=", 80), "\n")
  cat(paste0("SAVING STAGE ", stage_num, " CHECKPOINT\n"))
  cat(strrep("=", 80), "\n")
  cat(paste0("  Timestamp: ", checkpoint_timestamp, "\n"))
  cat(paste0("  Version: ", checkpoint_version, "\n\n"))

  # Define checkpoint directory in S3
  if (!exists("ORIGINAL_DATA_PATH", envir = .GlobalEnv) || !grepl("^s3://", ORIGINAL_DATA_PATH)) {
    cat("  WARNING: ORIGINAL_DATA_PATH not set or not S3, skipping checkpoint save\n\n")
    return(FALSE)
  }

  checkpoint_s3_path <- paste0(ORIGINAL_DATA_PATH, "/checkpoint/stage", stage_num, "/")

  # Capture environment variables
  checkpoint_env <- list(
    BY_TIER = if(exists("BY_TIER", envir = .GlobalEnv)) BY_TIER else NULL,
    MODEL_TYPE = if(exists("MODEL_TYPE", envir = .GlobalEnv)) MODEL_TYPE else NULL,
    DOMAIN_CATEGORY = if(exists("DOMAIN_CATEGORY", envir = .GlobalEnv)) DOMAIN_CATEGORY else NULL,
    BASIS_RESOLUTION = if(exists("BASIS_RESOLUTION", envir = .GlobalEnv)) BASIS_RESOLUTION else 3,
    DATA_PATH = if(exists("DATA_PATH", envir = .GlobalEnv)) DATA_PATH else DEFAULT_DATA_PATH,
    RDATA_FILE = if(exists("RDATA_FILE", envir = .GlobalEnv)) RDATA_FILE else NULL,
    AWS_PATH = if(exists("AWS_PATH", envir = .GlobalEnv)) AWS_PATH else "",
    ORIGINAL_DATA_PATH = if(exists("ORIGINAL_DATA_PATH", envir = .GlobalEnv)) ORIGINAL_DATA_PATH else "",
    DEBUG_MODE = if(exists("DEBUG_MODE", envir = .GlobalEnv)) DEBUG_MODE else FALSE,
    GENERATE_REPORT = if(exists("GENERATE_REPORT", envir = .GlobalEnv)) GENERATE_REPORT else FALSE,
    DATA_FROM = if(exists("DATA_FROM", envir = .GlobalEnv)) DATA_FROM else NULL,
    runStage = if(exists("runStage", envir = .GlobalEnv)) runStage else NULL
  )

  # Define files to checkpoint based on stage
  files_to_checkpoint <- list()

  if (stage_num == 1) {
    # Stage 1: Only environment variables (no files loaded yet)
    # startMatter() only validates paths and sets variables via parseCLA()
    # No data files are actually loaded in Stage 1
    files_to_checkpoint$primary <- c()
    files_to_checkpoint$processed <- c()

  } else if (stage_num == 2) {
    # Stage 2: Tier hierarchies and raw data loaded by model_loadData()
    files_to_checkpoint$primary <- c(
      paste0(DATA_PATH, "primary/tier2.sf.RData"),
      paste0(DATA_PATH, "primary/tier3.sf.RData"),
      paste0(DATA_PATH, "primary/tier4.sf.RData"),
      paste0(DATA_PATH, "primary/tier5.sf.RData"),
      paste0(DATA_PATH, "primary/reef_layer.sf.RData")
    )
    # Find the actual RData file in /data/raw/
    raw_files <- list.files(paste0(DATA_PATH, "raw"), pattern = "\\.RData$", full.names = TRUE)
    files_to_checkpoint$raw <- raw_files
    files_to_checkpoint$processed <- c()

  } else if (stage_num == 3) {
    # Stage 3: Data processed with covariates by model_processData()
    files_to_checkpoint$primary <- c(
      paste0(DATA_PATH, "primary/tiers.lookup.RData"),
      paste0(DATA_PATH, "primary/tier2.sf.RData"),
      paste0(DATA_PATH, "primary/tier3.sf.RData"),
      paste0(DATA_PATH, "primary/tier4.sf.RData"),
      paste0(DATA_PATH, "primary/tier5.sf.RData"),
      paste0(DATA_PATH, "primary/reef_layer.sf.RData")
    )
    files_to_checkpoint$processed <- c(
      paste0(DATA_PATH, "processed/reef_data_with_covariates.RData"),
      paste0(DATA_PATH, "processed/covariates_full_tier5.RData"),
      paste0(DATA_PATH, "processed/tiers.sf.RData")
    )
  }

  # Create checkpoint manifest
  checkpoint_info <- list(
    timestamp = checkpoint_timestamp,
    version = checkpoint_version,
    stage = stage_num,
    environment = checkpoint_env,
    parameters = list(
      domain = if(exists("DOMAIN_CATEGORY", envir = .GlobalEnv)) DOMAIN_CATEGORY else NULL,
      by_tier = if(exists("BY_TIER", envir = .GlobalEnv)) BY_TIER else NULL,
      model_type = if(exists("MODEL_TYPE", envir = .GlobalEnv)) MODEL_TYPE else NULL,
      basis_resolution = ifelse(exists("BASIS_RESOLUTION", envir = .GlobalEnv), BASIS_RESOLUTION, 3)
    ),
    files_included = unlist(lapply(files_to_checkpoint, basename)),
    status = "complete"
  )

  # Save manifest
  manifest_file <- paste0(DATA_PATH, "processed/checkpoint_stage", stage_num, "_manifest.rds")
  saveRDS(checkpoint_info, file = manifest_file)
  cat(paste0("  ✓ Created checkpoint manifest\n\n"))

  # Upload files to S3
  all_files <- c(manifest_file, unlist(files_to_checkpoint))

  if (length(all_files) == 1) {
    # Only manifest, no data files (e.g., Stage 1)
    cat("  Uploading manifest only (no data files at this stage):\n")
  } else {
    cat("  Uploading files to S3:\n")
  }

  success_count <- 0

  for (file in all_files) {
    if (file.exists(file)) {
      # Determine destination path maintaining directory structure
      if (grepl("/processed/", file)) {
        dest <- paste0(checkpoint_s3_path, "processed/", basename(file))
      } else if (grepl("/primary/", file)) {
        dest <- paste0(checkpoint_s3_path, "primary/", basename(file))
      } else if (grepl("/raw/", file)) {
        dest <- paste0(checkpoint_s3_path, "raw/", basename(file))
      } else {
        dest <- paste0(checkpoint_s3_path, basename(file))
      }

      # Upload using aws CLI
      cmd <- sprintf('aws s3 cp "%s" "%s" --quiet', file, dest)
      result <- system(cmd, intern = FALSE)

      if (result == 0) {
        file_size <- file.info(file)$size
        cat(sprintf("    ✓ %s (%.1f MB)\n", basename(file), file_size / 1024^2))
        success_count <- success_count + 1
      } else {
        cat(sprintf("    ✗ Failed to upload: %s\n", basename(file)))
      }
    } else {
      cat(sprintf("    ⚠ File not found: %s\n", basename(file)))
    }
  }

  cat("\n")
  cat(paste0("  Uploaded ", success_count, " of ", length(all_files), " files\n"))
  cat(paste0("  Checkpoint location: ", checkpoint_s3_path, "\n"))
  cat(strrep("=", 80), "\n\n")

  return(success_count == length(all_files))
}


#' @title Restore Stage Checkpoint from S3 or Local Files
#' @description Downloads checkpoint data from S3 for a given stage, or verifies
#'   local checkpoint files already exist when running without S3.
#' @param stage_num Integer (1, 2, or 3)
#' @return Logical indicating if checkpoint was successfully restored
#' @export
restore_stage_checkpoint <- function(stage_num) {

  cat("\n")
  cat(strrep("=", 80), "\n")
  cat(paste0("CHECKING FOR STAGE ", stage_num, " CHECKPOINT\n"))
  cat(strrep("=", 80), "\n")

  # Check if S3 path exists — if not, try local file restore
  if (!exists("ORIGINAL_DATA_PATH", envir = .GlobalEnv) || !grepl("^s3://", ORIGINAL_DATA_PATH)) {
    cat("  No S3 data path configured — checking for local checkpoint files...\n")
    return(restore_stage_checkpoint_local(stage_num))
  }

  checkpoint_s3_path <- paste0(ORIGINAL_DATA_PATH, "/checkpoint/stage", stage_num, "/")
  manifest_s3 <- paste0(checkpoint_s3_path, "processed/checkpoint_stage", stage_num, "_manifest.rds")

  cat(paste0("  Checking: ", checkpoint_s3_path, "\n\n"))

  # Check if checkpoint exists
  check_cmd <- sprintf('aws s3 ls "%s" 2>/dev/null | wc -l', checkpoint_s3_path)
  file_count <- as.numeric(system(check_cmd, intern = TRUE))

  if (file_count == 0) {
    cat(paste0("  No checkpoint found in S3 for stage ", stage_num, "\n"))
    cat("  Will run from beginning\n")
    cat(strrep("=", 80), "\n\n")
    return(FALSE)
  }

  # Download manifest
  manifest_local <- paste0(DATA_PATH, "processed/checkpoint_stage", stage_num, "_manifest.rds")
  dir.create(paste0(DATA_PATH, "processed"), recursive = TRUE, showWarnings = FALSE)

  download_cmd <- sprintf('aws s3 cp "%s" "%s" --quiet 2>/dev/null', manifest_s3, manifest_local)
  result <- system(download_cmd, intern = FALSE)

  if (result != 0 || !file.exists(manifest_local)) {
    cat(paste0("  Checkpoint manifest not found for stage ", stage_num, "\n"))
    cat(strrep("=", 80), "\n\n")
    return(FALSE)
  }

  # Read manifest
  checkpoint_info <- readRDS(manifest_local)

  cat("  Found checkpoint:\n")
  cat(paste0("    Timestamp: ", checkpoint_info$timestamp, "\n"))
  cat(paste0("    Version: ", checkpoint_info$version, "\n"))
  cat(paste0("    Stage: ", checkpoint_info$stage, "\n"))
  cat(paste0("    Files: ", length(checkpoint_info$files_included), "\n"))

  if (!is.null(checkpoint_info$parameters$domain)) {
    cat(paste0("    Parameters: domain=", checkpoint_info$parameters$domain,
               ", by_tier=", checkpoint_info$parameters$by_tier,
               ", model_type=", checkpoint_info$parameters$model_type, "\n"))

    # Warn if parameters differ from current run
    param_mismatch <- FALSE
    if (exists("BY_TIER", envir = .GlobalEnv) && !is.null(checkpoint_info$parameters$by_tier)) {
      if (BY_TIER != checkpoint_info$parameters$by_tier) {
        cat(paste0("    ⚠ WARNING: Current BY_TIER (", BY_TIER, ") differs from checkpoint (",
                   checkpoint_info$parameters$by_tier, ")\n"))
        param_mismatch <- TRUE
      }
    }
    if (exists("MODEL_TYPE", envir = .GlobalEnv) && !is.null(checkpoint_info$parameters$model_type)) {
      if (MODEL_TYPE != checkpoint_info$parameters$model_type) {
        cat(paste0("    ⚠ WARNING: Current MODEL_TYPE (", MODEL_TYPE, ") differs from checkpoint (",
                   checkpoint_info$parameters$model_type, ")\n"))
        param_mismatch <- TRUE
      }
    }
    if (param_mismatch) {
      cat("    ℹ Checkpoint was created with different parameters. Using current parameters.\n")
    }
  }

  # Restore environment variables (only those not already set by current session's
  # parseCLA/startMatter — runtime parameters like BASIS_RESOLUTION, MODEL_TYPE,
  # DEBUG_MODE take precedence over checkpoint-stored values)
  if (!is.null(checkpoint_info$environment)) {
    cat("\n  Restoring environment variables:\n")
    for (var_name in names(checkpoint_info$environment)) {
      if (!is.null(checkpoint_info$environment[[var_name]])) {
        if (exists(var_name, envir = .GlobalEnv)) {
          cat(sprintf("    \u229c %s (keeping current value)\n", var_name))
        } else {
          assign(var_name, checkpoint_info$environment[[var_name]], envir = .GlobalEnv)
          cat(sprintf("    \u2713 %s\n", var_name))
        }
      }
    }
  }

  # Download checkpoint files (if any)
  if (length(checkpoint_info$files_included) == 0) {
    # Stage 1 has no data files, only environment variables
    cat("\n  No data files to restore for this stage (environment variables only)\n")
    cat(paste0("  ✓ Successfully restored checkpoint\n"))
    cat(paste0("  Proceeding to Stage ", stage_num + 1, "\n"))
    cat(strrep("=", 80), "\n\n")
    return(TRUE)
  }

  cat("\n  Downloading checkpoint files from S3:\n")

  # Download all checkpoint files
  success_count <- 0
  for (file_name in checkpoint_info$files_included) {
    # Determine source and destination paths
    if (grepl("manifest", file_name)) {
      s3_file <- paste0(checkpoint_s3_path, "processed/", file_name)
      local_file <- paste0(DATA_PATH, "processed/", file_name)
      dir.create(paste0(DATA_PATH, "processed"), recursive = TRUE, showWarnings = FALSE)
    } else if (grepl("reef_data_with_covariates|covariates_full|tiers\\.sf", file_name)) {
      s3_file <- paste0(checkpoint_s3_path, "processed/", file_name)
      local_file <- paste0(DATA_PATH, "processed/", file_name)
      dir.create(paste0(DATA_PATH, "processed"), recursive = TRUE, showWarnings = FALSE)
    } else if (grepl("\\.RData$", file_name) && grepl("reef_data", file_name)) {
      s3_file <- paste0(checkpoint_s3_path, "raw/", file_name)
      local_file <- paste0(DATA_PATH, "raw/", file_name)
      dir.create(paste0(DATA_PATH, "raw"), recursive = TRUE, showWarnings = FALSE)
    } else {
      s3_file <- paste0(checkpoint_s3_path, "primary/", file_name)
      local_file <- paste0(DATA_PATH, "primary/", file_name)
      dir.create(paste0(DATA_PATH, "primary"), recursive = TRUE, showWarnings = FALSE)
    }

    # Download file
    dl_cmd <- sprintf('aws s3 cp "%s" "%s" --quiet 2>/dev/null', s3_file, local_file)
    result <- system(dl_cmd, intern = FALSE)

    if (result == 0 && file.exists(local_file)) {
      file_size <- file.info(local_file)$size
      cat(sprintf("    ✓ %s (%.1f MB)\n", file_name, file_size / 1024^2))
      success_count <- success_count + 1
    } else {
      cat(sprintf("    ✗ Failed to download: %s\n", file_name))
    }
  }

  cat("\n")
  if (success_count == length(checkpoint_info$files_included)) {
    cat(paste0("  ✓ Successfully restored checkpoint (", success_count, " files)\n"))
    cat(paste0("  Proceeding to Stage ", stage_num + 1, "\n"))
    cat(strrep("=", 80), "\n\n")
    return(TRUE)
  } else {
    cat(paste0("  ✗ Failed to restore all files (", success_count, "/",
                length(checkpoint_info$files_included), ")\n"))
    cat(strrep("=", 80), "\n\n")
    return(FALSE)
  }
}


#' @title Restore Stage Checkpoint from Local Files
#' @description Verifies that checkpoint data files already exist in DATA_PATH
#'   (e.g. from a previous run or pre-populated bind mount). Restores environment
#'   variables from manifest if available.
#' @param stage_num Integer (1, 2, or 3)
#' @return Logical indicating if all required files were found
#' @keywords internal
restore_stage_checkpoint_local <- function(stage_num) {

  data_path <- if (exists("DATA_PATH", envir = .GlobalEnv)) DATA_PATH else DEFAULT_DATA_PATH

  # Define required files per stage (same list as save_stage_checkpoint)
  if (stage_num == 2) {
    required_files <- c(
      paste0(data_path, "primary/tier2.sf.RData"),
      paste0(data_path, "primary/tier3.sf.RData"),
      paste0(data_path, "primary/tier4.sf.RData"),
      paste0(data_path, "primary/tier5.sf.RData"),
      paste0(data_path, "primary/reef_layer.sf.RData")
    )
  } else if (stage_num == 3) {
    required_files <- c(
      paste0(data_path, "primary/tiers.lookup.RData"),
      paste0(data_path, "primary/tier2.sf.RData"),
      paste0(data_path, "primary/tier3.sf.RData"),
      paste0(data_path, "primary/tier4.sf.RData"),
      paste0(data_path, "primary/tier5.sf.RData"),
      paste0(data_path, "primary/reef_layer.sf.RData"),
      paste0(data_path, "processed/reef_data_with_covariates.RData"),
      paste0(data_path, "processed/covariates_full_tier5.RData"),
      paste0(data_path, "processed/tiers.sf.RData")
    )
  } else {
    cat(paste0("  No local checkpoint definition for stage ", stage_num, "\n"))
    cat(strrep("=", 80), "\n\n")
    return(FALSE)
  }

  # Check which files exist
  cat(paste0("  Checking ", length(required_files), " required files in ", data_path, ":\n"))
  found <- 0
  missing <- character(0)
  for (f in required_files) {
    if (file.exists(f)) {
      file_size <- file.info(f)$size
      cat(sprintf("    ✓ %s (%.1f MB)\n", basename(f), file_size / 1024^2))
      found <- found + 1
    } else {
      cat(sprintf("    ✗ MISSING: %s\n", basename(f)))
      missing <- c(missing, f)
    }
  }

  if (length(missing) > 0) {
    cat(paste0("\n  ✗ Missing ", length(missing), " of ", length(required_files), " files\n"))
    cat(strrep("=", 80), "\n\n")
    return(FALSE)
  }

  # Try to restore environment variables from manifest if it exists
  manifest_file <- paste0(data_path, "processed/checkpoint_stage", stage_num, "_manifest.rds")
  if (file.exists(manifest_file)) {
    checkpoint_info <- readRDS(manifest_file)
    cat(paste0("\n  Manifest: version=", checkpoint_info$version,
               ", timestamp=", checkpoint_info$timestamp, "\n"))
    if (!is.null(checkpoint_info$environment)) {
      cat("  Restoring environment variables:\n")
      for (var_name in names(checkpoint_info$environment)) {
        if (!is.null(checkpoint_info$environment[[var_name]])) {
          if (exists(var_name, envir = .GlobalEnv)) {
            cat(sprintf("    \u229c %s (keeping current value)\n", var_name))
          } else {
            assign(var_name, checkpoint_info$environment[[var_name]], envir = .GlobalEnv)
            cat(sprintf("    \u2713 %s\n", var_name))
          }
        }
      }
    }
  } else {
    cat("\n  No manifest found — skipping environment variable restore\n")
    cat("  (Ensure parseCLA / startMatter has already set required globals)\n")
  }

  cat(paste0("\n  ✓ Local checkpoint verified (", found, " files)\n"))
  cat(paste0("  Proceeding to Stage ", stage_num + 1, "\n"))
  cat(strrep("=", 80), "\n\n")
  return(TRUE)
}
