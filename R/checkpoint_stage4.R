#' @title Save Stage 4 Group Checkpoint to S3
#' @description Saves checkpoint after completing modeling for one benthic group
#' @param checkpoint_name Character string like "ch_4.1a" (group + method)
#' @param group Character string: "HARD CORAL", "SOFT CORAL", or "MACROALGAE"
#' @param method Character string: "type5" or "type6"
#' @param tiers_status List containing status for each tier modeled
#' @return Logical indicating success
#' @export
save_group_checkpoint <- function(checkpoint_name, group, method, tiers_status) {

  checkpoint_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  checkpoint_version <- system.file("VERSION", package = "reefCloudPackage") %>%
    readLines(n = 1) %>%
    trimws()

  cat("\n")
  cat(strrep("=", 80), "\n")
  cat(paste0("SAVING GROUP CHECKPOINT: ", checkpoint_name, "\n"))
  cat(strrep("=", 80), "\n")
  cat(paste0("  Timestamp: ", checkpoint_timestamp, "\n"))
  cat(paste0("  Version: ", checkpoint_version, "\n"))
  cat(paste0("  Group: ", group, "\n"))
  cat(paste0("  Method: ", method, "\n\n"))

  # Validate S3 path
  if (!exists("ORIGINAL_DATA_PATH", envir = .GlobalEnv) || !grepl("^s3://", ORIGINAL_DATA_PATH)) {
    cat("  WARNING: ORIGINAL_DATA_PATH not set or not S3, skipping checkpoint save\n\n")
    return(FALSE)
  }

  checkpoint_s3_path <- paste0(ORIGINAL_DATA_PATH, "/checkpoint/", checkpoint_name, "/")

  # Count successful/failed tiers
  total_tiers <- length(tiers_status)
  successful_tiers <- sum(sapply(tiers_status, function(x) x$status == "complete"))
  failed_tiers <- total_tiers - successful_tiers

  cat(paste0("  Total tiers: ", total_tiers, "\n"))
  cat(paste0("  Successful: ", successful_tiers, "\n"))
  cat(paste0("  Failed: ", failed_tiers, "\n\n"))

  # Capture environment variables
  checkpoint_env <- list(
    BY_TIER = if(exists("BY_TIER", envir = .GlobalEnv)) BY_TIER else NULL,
    MODEL_TYPE = if(exists("MODEL_TYPE", envir = .GlobalEnv)) MODEL_TYPE else NULL,
    DOMAIN_CATEGORY = if(exists("DOMAIN_CATEGORY", envir = .GlobalEnv)) DOMAIN_CATEGORY else NULL,
    BASIS_RESOLUTION = if(exists("BASIS_RESOLUTION", envir = .GlobalEnv)) BASIS_RESOLUTION else 3,
    DATA_PATH = if(exists("DATA_PATH", envir = .GlobalEnv)) DATA_PATH else DEFAULT_DATA_PATH,
    ORIGINAL_DATA_PATH = if(exists("ORIGINAL_DATA_PATH", envir = .GlobalEnv)) ORIGINAL_DATA_PATH else ""
  )

  # Get list of model output files
  model_files <- list.files(paste0(DATA_PATH, "modelled"), pattern = "\\.RData$", full.names = FALSE)

  # Create checkpoint manifest
  checkpoint_info <- list(
    timestamp = checkpoint_timestamp,
    version = checkpoint_version,
    checkpoint = checkpoint_name,
    group = group,
    method = method,
    focal_tier = paste0('Tier', as.numeric(BY_TIER) - 1),
    environment = checkpoint_env,
    tiers_status = tiers_status,
    total_tiers = total_tiers,
    successful_tiers = successful_tiers,
    failed_tiers = failed_tiers,
    model_outputs = model_files,
    tier_checkpoints_deleted = FALSE,  # Will be updated by cleanup function
    status = "complete"
  )

  # Save manifest locally
  manifest_file <- paste0(DATA_PATH, "processed/", checkpoint_name, "_manifest.rds")
  dir.create(paste0(DATA_PATH, "processed"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(checkpoint_info, file = manifest_file)
  cat("  ✓ Created checkpoint manifest\n\n")

  # Upload manifest to S3
  cat("  Uploading checkpoint to S3:\n")
  manifest_dest <- paste0(checkpoint_s3_path, "manifest.rds")
  manifest_cmd <- sprintf('aws s3 cp "%s" "%s" --quiet', manifest_file, manifest_dest)
  manifest_result <- system(manifest_cmd, intern = FALSE)

  if (manifest_result == 0) {
    cat(sprintf("    ✓ manifest.rds\n"))
  } else {
    cat(sprintf("    ✗ Failed to upload manifest\n"))
    return(FALSE)
  }

  # Upload model output files
  cat("  Uploading model outputs:\n")
  success_count <- 0

  for (model_file in model_files) {
    local_path <- paste0(DATA_PATH, "modelled/", model_file)
    if (file.exists(local_path)) {
      dest <- paste0(checkpoint_s3_path, "modelled/", model_file)
      cmd <- sprintf('aws s3 cp "%s" "%s" --quiet', local_path, dest)
      result <- system(cmd, intern = FALSE)

      if (result == 0) {
        file_size <- file.info(local_path)$size
        cat(sprintf("    ✓ %s (%.1f MB)\n", model_file, file_size / 1024^2))
        success_count <- success_count + 1
      } else {
        cat(sprintf("    ✗ Failed: %s\n", model_file))
      }
    }
  }

  cat("\n")
  cat(paste0("  Uploaded ", success_count, " of ", length(model_files), " model files\n"))
  cat(paste0("  Checkpoint location: ", checkpoint_s3_path, "\n"))
  cat(strrep("=", 80), "\n\n")

  # Cleanup tier-level checkpoints
  cleanup_success <- cleanup_tier_checkpoints(checkpoint_name)
  if (cleanup_success) {
    # Update manifest with cleanup status
    checkpoint_info$tier_checkpoints_deleted <- TRUE
    saveRDS(checkpoint_info, file = manifest_file)
    cmd <- sprintf('aws s3 cp "%s" "%s" --quiet', manifest_file, manifest_dest)
    system(cmd, intern = FALSE)
  }

  return(success_count == length(model_files))
}


#' @title Save Tier-Level Sub-Checkpoint (for debugging)
#' @description Saves intermediate data for a specific tier at a specific step
#' @param checkpoint_name Character string like "ch_4.1a"
#' @param tier Character string like "1808"
#' @param step Integer 1-4 indicating which step
#' @param data_list List containing objects to save for this step
#' @return Logical indicating success
#' @export
save_tier_checkpoint <- function(checkpoint_name, tier, step, data_list) {

  if (!exists("ORIGINAL_DATA_PATH", envir = .GlobalEnv) || !grepl("^s3://", ORIGINAL_DATA_PATH)) {
    return(FALSE)
  }

  checkpoint_s3_path <- paste0(ORIGINAL_DATA_PATH, "/checkpoint/", checkpoint_name, "/tier_", tier, "/")

  # Create step filename
  step_names <- c("step1_covariates", "step2_frk_inputs", "step3_model", "step4_predictions")
  step_file <- paste0(step_names[step], ".RData")
  local_path <- paste0(DATA_PATH, "processed/tier_", tier, "_", step_file)

  # Save data locally
  dir.create(paste0(DATA_PATH, "processed"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(data_list, file = local_path)

  # Upload to S3
  dest <- paste0(checkpoint_s3_path, step_file)
  cmd <- sprintf('aws s3 cp "%s" "%s" --quiet', local_path, dest)
  result <- system(cmd, intern = FALSE)

  # Clean up local file
  unlink(local_path)

  return(result == 0)
}


#' @title Cleanup Tier-Level Checkpoints
#' @description Deletes tier-level checkpoint folders after group checkpoint succeeds
#' @param checkpoint_name Character string like "ch_4.1a"
#' @return Logical indicating success
#' @export
cleanup_tier_checkpoints <- function(checkpoint_name) {

  if (!exists("ORIGINAL_DATA_PATH", envir = .GlobalEnv) || !grepl("^s3://", ORIGINAL_DATA_PATH)) {
    return(FALSE)
  }

  checkpoint_s3_path <- paste0(ORIGINAL_DATA_PATH, "/checkpoint/", checkpoint_name, "/")

  cat("  Cleaning up tier-level checkpoints...\n")

  # List all tier folders
  list_cmd <- sprintf('aws s3 ls "%s" --recursive', checkpoint_s3_path)
  tier_files <- system(list_cmd, intern = TRUE)

  # Find tier_* folders
  tier_folders <- unique(gsub("^.*(tier_\\d+)/.*$", "\\1", tier_files[grepl("tier_\\d+", tier_files)]))

  if (length(tier_folders) == 0) {
    cat("    (no tier checkpoints to clean up)\n")
    return(TRUE)
  }

  # Delete each tier folder
  success_count <- 0
  for (tier_folder in tier_folders) {
    delete_path <- paste0(checkpoint_s3_path, tier_folder, "/")
    delete_cmd <- sprintf('aws s3 rm "%s" --recursive --quiet', delete_path)
    result <- system(delete_cmd, intern = FALSE)

    if (result == 0) {
      success_count <- success_count + 1
    }
  }

  cat(sprintf("    ✓ Deleted %d tier checkpoint folders\n", success_count))

  return(success_count == length(tier_folders))
}


#' @title Restore Group Checkpoint
#' @description Restores data from a group-level checkpoint
#' @param checkpoint_name Character string like "ch_4.1a"
#' @return Logical indicating success
#' @export
restore_group_checkpoint <- function(checkpoint_name) {

  cat("\n")
  cat(strrep("=", 80), "\n")
  cat(paste0("RESTORING GROUP CHECKPOINT: ", checkpoint_name, "\n"))
  cat(strrep("=", 80), "\n")

  if (!exists("ORIGINAL_DATA_PATH", envir = .GlobalEnv) || !grepl("^s3://", ORIGINAL_DATA_PATH)) {
    cat("  No S3 data path configured\n")
    cat(strrep("=", 80), "\n\n")
    return(FALSE)
  }

  checkpoint_s3_path <- paste0(ORIGINAL_DATA_PATH, "/checkpoint/", checkpoint_name, "/")

  cat(paste0("  Checking: ", checkpoint_s3_path, "\n\n"))

  # Check if checkpoint exists
  check_cmd <- sprintf('aws s3 ls "%s" 2>/dev/null | wc -l', checkpoint_s3_path)
  file_count <- as.numeric(system(check_cmd, intern = TRUE))

  if (file_count == 0) {
    cat(paste0("  No checkpoint found: ", checkpoint_name, "\n"))
    cat(strrep("=", 80), "\n\n")
    return(FALSE)
  }

  # Download manifest
  manifest_s3 <- paste0(checkpoint_s3_path, "manifest.rds")
  manifest_local <- paste0(DATA_PATH, "processed/", checkpoint_name, "_manifest.rds")
  dir.create(paste0(DATA_PATH, "processed"), recursive = TRUE, showWarnings = FALSE)

  download_cmd <- sprintf('aws s3 cp "%s" "%s" --quiet 2>/dev/null', manifest_s3, manifest_local)
  result <- system(download_cmd, intern = FALSE)

  if (result != 0 || !file.exists(manifest_local)) {
    cat("  Checkpoint manifest not found\n")
    cat(strrep("=", 80), "\n\n")
    return(FALSE)
  }

  # Read manifest
  checkpoint_info <- readRDS(manifest_local)

  cat("  Found checkpoint:\n")
  cat(paste0("    Timestamp: ", checkpoint_info$timestamp, "\n"))
  cat(paste0("    Version: ", checkpoint_info$version, "\n"))
  cat(paste0("    Group: ", checkpoint_info$group, "\n"))
  cat(paste0("    Method: ", checkpoint_info$method, "\n"))
  cat(paste0("    Tiers: ", checkpoint_info$successful_tiers, " successful, ",
             checkpoint_info$failed_tiers, " failed\n"))
  cat(paste0("    Model files: ", length(checkpoint_info$model_outputs), "\n\n"))

  # Restore environment variables
  if (!is.null(checkpoint_info$environment)) {
    for (var_name in names(checkpoint_info$environment)) {
      if (!is.null(checkpoint_info$environment[[var_name]])) {
        assign(var_name, checkpoint_info$environment[[var_name]], envir = .GlobalEnv)
      }
    }
  }

  # Download model output files
  cat("  Downloading model outputs from S3:\n")
  dir.create(paste0(DATA_PATH, "modelled"), recursive = TRUE, showWarnings = FALSE)

  success_count <- 0
  for (model_file in checkpoint_info$model_outputs) {
    s3_file <- paste0(checkpoint_s3_path, "modelled/", model_file)
    local_file <- paste0(DATA_PATH, "modelled/", model_file)

    dl_cmd <- sprintf('aws s3 cp "%s" "%s" --quiet 2>/dev/null', s3_file, local_file)
    result <- system(dl_cmd, intern = FALSE)

    if (result == 0 && file.exists(local_file)) {
      file_size <- file.info(local_file)$size
      cat(sprintf("    ✓ %s (%.1f MB)\n", model_file, file_size / 1024^2))
      success_count <- success_count + 1
    } else {
      cat(sprintf("    ✗ Failed: %s\n", model_file))
    }
  }

  cat("\n")
  if (success_count == length(checkpoint_info$model_outputs)) {
    cat(paste0("  ✓ Successfully restored checkpoint (", success_count, " files)\n"))
    cat(strrep("=", 80), "\n\n")
    return(TRUE)
  } else {
    cat(paste0("  ✗ Failed to restore all files (", success_count, "/",
               length(checkpoint_info$model_outputs), ")\n"))
    cat(strrep("=", 80), "\n\n")
    return(FALSE)
  }
}
