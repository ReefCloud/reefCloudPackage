#' @title Debug utility functions
#' @description Helper functions for debug logging and S3 upload
#' @author Julie Vercelloni

#' Upload debug data to S3 with compression
#' @param local_path Local file path to upload
#' @param s3_key Object key relative to the run's debug/ prefix
#' @param compress Whether to gzip the file before upload (default TRUE)
#' @param dest S3 destination URI (default: $ORIGINAL_DATA_PATH from the
#'   container env). Pass an explicit s3:// URI to override. If the
#'   destination isn't an s3:// URI (e.g. local development run) the
#'   upload is silently skipped.
#' @return TRUE if uploaded, FALSE otherwise (including on skip)
upload_debug_to_s3 <- function(local_path, s3_key, compress = TRUE, dest = NULL) {

  if (is.null(dest)) {
    dest <- Sys.getenv("ORIGINAL_DATA_PATH", unset = "")
  }

  # Skip silently when there is no S3 target — local development runs.
  if (!nzchar(dest) || !startsWith(dest, "s3://")) {
    return(invisible(FALSE))
  }

  if (!file.exists(local_path)) {
    warning(paste("Debug file does not exist:", local_path))
    return(FALSE)
  }

  # Compress if requested
  if (compress && !grepl("\\.gz$", local_path)) {
    compressed_path <- paste0(local_path, ".gz")
    tryCatch({
      system(paste0("gzip -f ", shQuote(local_path)), wait = TRUE)
      local_path <- compressed_path
      s3_key <- paste0(s3_key, ".gz")
    }, error = function(e) {
      warning(paste("Failed to compress file:", e$message))
      return(FALSE)
    })
  }

  # Upload to S3 under <ORIGINAL_DATA_PATH>/debug/<s3_key>
  s3_full_path <- paste0(sub("/$", "", dest), "/debug/", s3_key)
  cmd <- paste0("aws s3 cp ", shQuote(local_path), " ", shQuote(s3_full_path))

  result <- tryCatch({
    system(cmd, wait = TRUE)
  }, error = function(e) {
    warning(paste("Failed to upload to S3:", e$message))
    return(1)
  })

  if (result == 0) {
    cat(paste0("✓ Uploaded debug file to: ", s3_full_path, "\n"))
    return(TRUE)
  } else {
    warning(paste("S3 upload failed with exit code:", result))
    return(FALSE)
  }
}

#' Save debug CSV and upload to S3
#' @param data Data frame to save
#' @param local_dir Local directory for debug files
#' @param filename Base filename (without .csv extension)
#' @param tier Current tier identifier
#' @param focal_tier Focal tier name
#' @param stage Stage/step identifier
#' @return TRUE if successful
save_debug_csv <- function(data, local_dir, filename, tier, focal_tier, stage) {

  # Create debug directory if it doesn't exist
  if (!dir.exists(local_dir)) {
    dir.create(local_dir, recursive = TRUE)
  }

  # Create explanatory filename
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  full_filename <- paste0(
    "debug_type6_",
    stage, "_",
    filename, "_",
    focal_tier, "_", tier, "_",
    timestamp,
    ".csv"
  )

  local_path <- file.path(local_dir, full_filename)

  # Save CSV
  tryCatch({
    write.csv(data, local_path, row.names = FALSE)
    cat(paste0("✓ Saved debug CSV: ", local_path, " (", nrow(data), " rows)\n"))

    # Upload to S3
    s3_key <- paste0("type6/", full_filename)
    upload_debug_to_s3(local_path, s3_key, compress = TRUE)

    return(TRUE)
  }, error = function(e) {
    warning(paste("Failed to save debug CSV:", e$message))
    return(FALSE)
  })
}

#' Log pipeline step with details
#' @param step_num Step number
#' @param step_name Step description
#' @param details Named list of details to log
log_pipeline_step <- function(step_num, step_name, details = list()) {
  cat(paste0("\n", strrep("=", 80), "\n"))
  cat(paste0("STEP ", step_num, ": ", step_name, "\n"))
  cat(paste0(strrep("=", 80), "\n"))

  for (name in names(details)) {
    value <- details[[name]]
    if (is.logical(value) || is.numeric(value) || is.character(value)) {
      if (length(value) == 1) {
        cat(paste0("  ", name, ": ", value, "\n"))
      } else if (length(value) <= 5) {
        cat(paste0("  ", name, ": ", paste(value, collapse = ", "), "\n"))
      } else {
        cat(paste0("  ", name, ": [", length(value), " values]\n"))
      }
    }
  }
  cat("\n")
}
