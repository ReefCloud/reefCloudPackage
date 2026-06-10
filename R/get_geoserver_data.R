#' @title Get Data from Geoserver
#' @description Downloads and filters spatial data from the geoserver for a specified tier and coverage name.
#' Includes adaptive chunking, retry logic, timeout handling, and progress indication.
#'
#' @param Tier An integer indicating the tier of the data to be retrieved.
#' @param cov_name A string indicating the name of the coverage to be retrieved.
#' @param rc_client WFS client object from get_geoserver_info()
#' @param timeout_seconds Timeout in seconds for each request (default: GEOSERVER_DEFAULT_TIMEOUT)
#' @param initial_chunk_size Initial chunk size for pagination (default: GEOSERVER_DEFAULT_CHUNK_SIZE)
#' @return A spatial data frame containing the filtered data from the geoserver.
#' @details Requires BY_TIER, DATA_PATH, and rc_client to be available in the environment.
#' Implements adaptive chunk sizing, exponential backoff retry, and progress indication.
#' @author Murray Logan
#' @examples
#' get_geoserver_info()
#' cov_data <- get_geoserver_data(Tier = 4, cov_name = "degrees_heating_weeks_tier", rc_client)
#' @export
get_geoserver_data <- function(Tier = as.numeric(BY_TIER) - 1,
                                cov_name = NULL,
                                rc_client,
                                use_cache = TRUE,
                                timeout_seconds = GEOSERVER_DEFAULT_TIMEOUT,
                                initial_chunk_size = GEOSERVER_DEFAULT_CHUNK_SIZE) {
  # Note: No status_try_catch wrapper because this function is called from parallel workers
  # where status tracking is not available. Error handling is done at the calling level.

  # Cache short-circuit: if a fresh-enough cache exists, return it without hitting geoserver.
  if (isTRUE(use_cache) && is_geoserver_cache_valid(cov_name, Tier)) {
    cached_data <- load_geoserver_cache(cov_name, Tier)
    if (!is.null(cached_data)) return(cached_data)
  }

  # Use cached tier data (Suggestion 29)
  tier.sf <- reefCloudPackage::get_tier_data(as.numeric(BY_TIER) - 1)

  wch_tier_id <- unique(dplyr::pull(tier.sf, tier_id))
  bbox <- paste(as.character(sf::st_bbox(tier.sf)), collapse = ',')

  # Adaptive chunking variables (Suggestion 6)
  chunk_size <- initial_chunk_size
  start_index <- 0
  all_features <- list()
  fetch_more <- TRUE
  timeout_count <- 0
  max_timeouts <- 3
  min_chunk_size <- 1000

  # Safety valve: Maximum chunks to prevent infinite loops
  max_chunks <- 200

  # Duplicate detection: Track last chunk hash to detect pagination failures
  last_chunk_hash <- NULL

  # Progress tracking (Suggestion 9)
  total_fetched <- 0
  chunk_number <- 0

  if (!DEBUG_MODE) {
    cli::cli_alert_info(paste0("Downloading ", cov_name, " (chunk size: ", chunk_size, ")"))
  }

  while(fetch_more && chunk_number < max_chunks) {
    chunk_number <- chunk_number + 1
    retry_count <- 0
    chunk_success <- FALSE

    # Retry loop with exponential backoff (Suggestion 8)
    while (retry_count <= GEOSERVER_MAX_RETRIES && !chunk_success) {
      tryCatch({
        # Set timeout for this request (Suggestion 7)
        chunk_data <- R.utils::withTimeout({
          # Suppress verbose output by capturing it and discarding
          invisible(capture.output({
            chunk_data <- rc_client$getFeatures(
              cov_name,
              srsName = "EPSG:4326",
              bbox = bbox,
              count = chunk_size,
              startIndex = start_index
            )
          }))
          # Return the actual data (assigned inside capture.output)
          chunk_data
        }, timeout = timeout_seconds, onTimeout = "error")

        # Success - process the chunk
        # Defensive check: ensure chunk_data is valid and has rows
        chunk_valid <- !is.null(chunk_data) &&
                      is.data.frame(chunk_data) &&
                      !is.na(nrow(chunk_data)) &&
                      nrow(chunk_data) > 0

        if (isTRUE(chunk_valid)) {
          # Duplicate detection: Check if we're getting the same data repeatedly
          # For sf objects, use attribute columns only (not geometry)
          # Create fingerprint: nrow + first/last row attribute values

          # Initialize to avoid "missing value where TRUE/FALSE needed" error
          current_chunk_hash <- NULL

          tryCatch({
            # Get non-geometry columns for sf objects
            if (inherits(chunk_data, "sf")) {
              attr_data <- sf::st_drop_geometry(chunk_data)
              attr_cols <- names(attr_data)[1:min(3, ncol(attr_data))]
            } else {
              attr_data <- chunk_data
              attr_cols <- names(chunk_data)[1:min(3, ncol(chunk_data))]
            }

            current_chunk_hash <- paste0(
              nrow(chunk_data), "_",
              paste(as.character(attr_data[1, attr_cols]), collapse="_"), "_",
              paste(as.character(attr_data[nrow(attr_data), attr_cols]), collapse="_")
            )
          }, error = function(e) {
            # Fallback: use simple row count hash
            current_chunk_hash <<- paste0("fallback_", nrow(chunk_data))
          })

          if (!is.null(last_chunk_hash) && !is.null(current_chunk_hash) && current_chunk_hash == last_chunk_hash) {
            # Same data returned twice - pagination not working
            if (!DEBUG_MODE) {
              cli::cli_alert_warning(paste0(
                "Duplicate data detected at chunk ", chunk_number, ". ",
                "Geoserver pagination may not be working. Stopping download."
              ))
            }
            fetch_more <- FALSE
            chunk_success <- TRUE
          } else {
            # New unique data
            all_features[[length(all_features) + 1]] <- chunk_data
            total_fetched <- total_fetched + nrow(chunk_data)
            last_chunk_hash <- current_chunk_hash

            # Progress indication (Suggestion 9)
            if (!DEBUG_MODE) {
              cli::cli_alert_success(paste0(
                "Chunk ", chunk_number, ": ", nrow(chunk_data), " features ",
                "(total: ", total_fetched, ")"
              ))
            }

            # Reset timeout counter on success
            timeout_count <- 0

            # Check if we're done
            n_rows <- nrow(chunk_data)
            if (!is.na(n_rows) && n_rows < chunk_size) {
              fetch_more <- FALSE
            } else {
              start_index <- start_index + chunk_size
            }
            chunk_success <- TRUE
          }

        } else {
          # No more data or invalid data
          if (!DEBUG_MODE) {
            if (is.null(chunk_data)) {
              cli::cli_alert_warning("Chunk returned NULL")
            } else if (!is.data.frame(chunk_data)) {
              # Print actual content to see error message
              char_preview <- if (is.character(chunk_data)) {
                substr(paste(chunk_data, collapse=" "), 1, 500)
              } else {
                "Not character type"
              }
              cli::cli_alert_warning(paste0(
                "Chunk is not a data.frame, got: ", class(chunk_data)[1]
              ))
              cli::cli_alert_info(paste0("Response content (first 500 chars): ", char_preview))
            } else if (is.na(nrow(chunk_data))) {
              cli::cli_alert_warning("Chunk nrow is NA")
            } else {
              cli::cli_alert_info(paste0("Chunk has ", nrow(chunk_data), " rows (stopping)"))
            }
          }
          fetch_more <- FALSE
          chunk_success <- TRUE
        }

      }, TimeoutException = function(e) {
        # Timeout occurred (Suggestion 6, 7, 8)
        retry_count <- retry_count + 1
        timeout_count <- timeout_count + 1

        if (retry_count <= GEOSERVER_MAX_RETRIES) {
          # Exponential backoff (Suggestion 8)
          wait_time <- GEOSERVER_BACKOFF_BASE ^ retry_count

          if (!DEBUG_MODE) {
            cli::cli_alert_warning(paste0(
              "Timeout on chunk ", chunk_number, " (attempt ", retry_count, "/", GEOSERVER_MAX_RETRIES, "). ",
              "Waiting ", wait_time, "s before retry..."
            ))
          }

          Sys.sleep(wait_time)

          # Adaptive chunk sizing: reduce chunk size after timeouts (Suggestion 6)
          if (timeout_count >= 2 && chunk_size > min_chunk_size) {
            old_chunk_size <- chunk_size
            chunk_size <- max(min_chunk_size, as.integer(chunk_size / 2))

            if (!DEBUG_MODE) {
              cli::cli_alert_info(paste0(
                "Reducing chunk size: ", old_chunk_size, " -> ", chunk_size
              ))
            }
          }
        } else {
          # Max retries exceeded
          if (!DEBUG_MODE) {
            cli::cli_alert_danger(paste0(
              "Failed after ", GEOSERVER_MAX_RETRIES, " retries on chunk ", chunk_number
            ))
          }
          fetch_more <- FALSE
          chunk_success <- TRUE  # Exit retry loop
        }

      }, error = function(e) {
        # Other errors
        retry_count <- retry_count + 1

        if (retry_count <= GEOSERVER_MAX_RETRIES) {
          # Exponential backoff
          wait_time <- GEOSERVER_BACKOFF_BASE ^ retry_count

          if (!DEBUG_MODE) {
            cli::cli_alert_warning(paste0(
              "Error on chunk ", chunk_number, " (attempt ", retry_count, "/", GEOSERVER_MAX_RETRIES, "): ",
              conditionMessage(e), ". Retrying in ", wait_time, "s..."
            ))
          }

          Sys.sleep(wait_time)
        } else {
          # Fallback: try fetching all at once if we haven't fetched anything yet
          if (start_index == 0 && length(all_features) == 0) {
            if (!DEBUG_MODE) {
              cli::cli_alert_warning("Attempting to fetch all features at once...")
            }

            tryCatch({
              # Suppress verbose output
              invisible(capture.output({
                chunk_data <- rc_client$getFeatures(cov_name, srsName = "EPSG:4326", bbox= bbox)
              }))
              if (!is.null(chunk_data)) {
                all_features[[1]] <- chunk_data
                total_fetched <- nrow(chunk_data)

                if (!DEBUG_MODE) {
                  cli::cli_alert_success(paste0("Fetched all ", total_fetched, " features"))
                }
              }
            }, error = function(e2) {
              if (!DEBUG_MODE) {
                cli::cli_alert_danger(paste0("Fallback also failed: ", conditionMessage(e2)))
              }
            })
          }

          fetch_more <- FALSE
          chunk_success <- TRUE
        }
      })
    }
  }

  # Check if maximum chunks limit was reached (safety valve)
  if (chunk_number >= max_chunks) {
    if (!DEBUG_MODE) {
      cli::cli_alert_warning(paste0(
        "Maximum chunk limit reached (", max_chunks, " chunks). ",
        "This may indicate a pagination issue with the geoserver. ",
        "Downloaded ", total_fetched, " features so far."
      ))
    }
  }

  # Combine all chunks efficiently (Suggestion 10)
  if (length(all_features) > 0) {
    if (!DEBUG_MODE) {
      cli::cli_alert_info(paste0("Combining ", length(all_features), " chunks..."))
    }

    # Use data.table::rbindlist if available for better performance
    if (requireNamespace("data.table", quietly = TRUE)) {
      cov_data <- data.table::rbindlist(all_features, use.names = TRUE, fill = TRUE)
      cov_data <- sf::st_as_sf(cov_data)
    } else {
      cov_data <- do.call(rbind, all_features)
    }

    if (!DEBUG_MODE) {
      cli::cli_alert_success(paste0(
        "Successfully downloaded ", nrow(cov_data), " features for ", cov_name
      ))
    }

    # Persist to cache for the next run.
    if (isTRUE(use_cache) && !is.null(cov_data) && nrow(cov_data) > 0) {
      save_geoserver_cache(cov_data, cov_name, Tier)
    }
  } else {
    cov_data <- NULL
    if (!DEBUG_MODE) {
      cli::cli_alert_warning(paste0("No data retrieved for ", cov_name))
    }
  }

  # Return the data
  return(cov_data)
}
