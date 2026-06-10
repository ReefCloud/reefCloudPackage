#' @title Validate Data Paths
#' @description Validates that AWS_PATH and DATA_PATH exist and contain required files.
#' Checks for symlinks and resolves them for clarity. Validates that required input
#' files are present before proceeding with the pipeline.
#' @return TRUE if all paths and files are valid, stops with error if validation fails
#' @details This function should be called early in the pipeline (after parseCLA and
#' generateSettings) to ensure all required paths and files are accessible before
#' processing begins. It provides clear error messages indicating which files or
#' directories are missing.
#' @examples
#' \dontrun{
#' # After setting AWS_PATH and DATA_PATH
#' validate_paths()
#' }
#' @author Murray Logan
#' @export
validate_paths <- function() {
  status::status_try_catch(
  {
    # Check that required global variables exist
    if (!exists("AWS_PATH")) {
      stop("AWS_PATH is not defined. Run parseCLA() first.")
    }
    if (!exists("DATA_PATH")) {
      stop("DATA_PATH is not defined. Run generateSettings() first.")
    }

    # Check AWS_PATH exists (may be symlink)
    if (!dir.exists(AWS_PATH)) {
      stop(paste0(
        "AWS_PATH does not exist: ", AWS_PATH, "\n",
        "Please check the --bucket argument or create the directory."
      ))
    }

    # Resolve symlinks for display and logging
    aws_real <- normalizePath(AWS_PATH, mustWork = TRUE)
    if (aws_real != AWS_PATH) {
      if (!DEBUG_MODE) {
        cli::cli_alert_info(paste0("AWS_PATH: ", AWS_PATH, " -> ", aws_real))
      }
    } else {
      if (!DEBUG_MODE) {
        cli::cli_alert_info(paste0("AWS_PATH: ", aws_real))
      }
    }

    # Check for raw data directory
    raw_path <- paste0(AWS_PATH, "raw/")
    if (!dir.exists(raw_path)) {
      stop(paste0(
        "Raw data directory does not exist: ", raw_path, "\n",
        "Expected structure: ", AWS_PATH, "raw/\n",
        "Please ensure input data is in the correct location."
      ))
    }

    # Check for required raw data files
    required_raw_files <- c(
      "reef_data.zip"
    )

    # Optional files that may be present
    optional_tier_files <- c(
      "tier-2.json",
      "tier-3.json",
      "tier-4.json",
      "tier-5.json",
      "tiers.zip"
    )

    missing_required <- c()
    for (file in required_raw_files) {
      full_path <- paste0(AWS_PATH, "raw/", file)
      if (!file.exists(full_path)) {
        missing_required <- c(missing_required, full_path)
      }
    }

    if (length(missing_required) > 0) {
      stop(paste0(
        "Required raw data files not found:\n",
        paste("  - ", missing_required, collapse = "\n"),
        "\n\nExpected files in: ", AWS_PATH, "raw/"
      ))
    }

    # Check for tier files if domain is tier
    if (exists("DOMAIN_CATEGORY") && DOMAIN_CATEGORY == "tier") {
      missing_tier <- c()
      for (file in optional_tier_files) {
        full_path <- paste0(AWS_PATH, "raw/", file)
        if (!file.exists(full_path)) {
          missing_tier <- c(missing_tier, file)
        }
      }
      if (length(missing_tier) > 0 && !DEBUG_MODE) {
        cli::cli_alert_warning(paste0(
          "Some tier files not found (may be generated or optional): ",
          paste(missing_tier, collapse = ", ")
        ))
      }
    }

    # Check DATA_PATH exists or can be created
    if (!dir.exists(DATA_PATH)) {
      tryCatch({
        dir.create(DATA_PATH, recursive = TRUE)
        if (!DEBUG_MODE) {
          cli::cli_alert_success(paste0("Created DATA_PATH: ", DATA_PATH))
        }
      }, error = function(e) {
        stop(paste0(
          "Cannot create DATA_PATH: ", DATA_PATH, "\n",
          "Error: ", e$message, "\n",
          "Please check directory permissions."
        ))
      })
    }

    # Resolve DATA_PATH symlinks
    data_real <- normalizePath(DATA_PATH, mustWork = TRUE)
    if (data_real != DATA_PATH) {
      if (!DEBUG_MODE) {
        cli::cli_alert_info(paste0("DATA_PATH: ", DATA_PATH, " -> ", data_real))
      }
    } else {
      if (!DEBUG_MODE) {
        cli::cli_alert_info(paste0("DATA_PATH: ", data_real))
      }
    }

    # Check if AWS_PATH and DATA_PATH point to same location
    if (aws_real == data_real) {
      if (!DEBUG_MODE) {
        cli::cli_alert_success(
          "AWS_PATH and DATA_PATH use the same location (unified configuration)"
        )
      }
    } else {
      if (!DEBUG_MODE) {
        cli::cli_alert_info(paste0(
          "Separate paths:\n",
          "  Input:  ", aws_real, "\n",
          "  Output: ", data_real
        ))
      }
    }

    # Check write permissions for DATA_PATH
    test_file <- paste0(DATA_PATH, ".write_test_", Sys.getpid())
    tryCatch({
      file.create(test_file)
      file.remove(test_file)
    }, error = function(e) {
      stop(paste0(
        "DATA_PATH is not writable: ", DATA_PATH, "\n",
        "Error: ", e$message, "\n",
        "Please check directory permissions."
      ))
    })

    # All validations passed
    if (!DEBUG_MODE) {
      cli::cli_alert_success("Path validation completed successfully")
    }
    return(TRUE)
  },
  stage_ = 1,
  name_ = "Validate paths",
  item_ = "validate_paths"
  )
}
