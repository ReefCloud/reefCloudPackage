#' Geoserver Data Caching Functions
#'
#' File-based caching for geoserver downloads. Cached data is reused for up to
#' GEOSERVER_CACHE_DAYS (default 7 days) to avoid repeated network fetches.
#' Cache files live alongside other intermediate data in DATA_PATH/primary/.

#' @title Check if Geoserver Cache is Valid
#' @description TRUE if cache file exists and is younger than `cache_days`.
#' @param cov_name Coverage name (e.g. "reefcloud:degrees_heating_weeks_tier")
#' @param tier_level Tier level (integer)
#' @param cache_days Cache validity in days (default GEOSERVER_CACHE_DAYS)
#' @return Logical
#' @export
is_geoserver_cache_valid <- function(cov_name, tier_level, cache_days = GEOSERVER_CACHE_DAYS) {
  safe_name <- gsub("[:/]", "_", cov_name)
  cache_file <- paste0(DATA_PATH, "primary/cache_", safe_name, "_tier", tier_level, ".RData")

  if (!file.exists(cache_file)) return(FALSE)

  file_age_days <- as.numeric(difftime(Sys.time(), file.info(cache_file)$mtime, units = "days"))
  is_valid <- file_age_days < cache_days

  if (is_valid) {
    cli::cli_alert_info(paste0(
      "Geoserver cache hit: ", cov_name, " (tier ", tier_level,
      ", age ", round(file_age_days, 1), "d)"
    ))
  } else {
    cli::cli_alert_info(paste0(
      "Geoserver cache expired: ", cov_name, " (tier ", tier_level,
      ", age ", round(file_age_days, 1), "d > ", cache_days, "d)"
    ))
  }

  is_valid
}

#' @title Load Cached Geoserver Data
#' @param cov_name Coverage name
#' @param tier_level Tier level
#' @return Cached object, or NULL on miss/error
#' @export
load_geoserver_cache <- function(cov_name, tier_level) {
  safe_name <- gsub("[:/]", "_", cov_name)
  cache_file <- paste0(DATA_PATH, "primary/cache_", safe_name, "_tier", tier_level, ".RData")

  if (!file.exists(cache_file)) return(NULL)

  tryCatch({
    cached_env <- new.env()
    load(cache_file, envir = cached_env)
    if (length(ls(cached_env)) == 0) return(NULL)
    cache_data <- cached_env[[ls(cached_env)[1]]]
    cli::cli_alert_success(paste0(
      "Loaded cached geoserver data: ", cov_name, " (tier ", tier_level,
      ", rows ", nrow(cache_data), ")"
    ))
    cache_data
  }, error = function(e) {
    cli::cli_alert_warning(paste0(
      "Failed to load geoserver cache for ", cov_name, ": ", conditionMessage(e)
    ))
    NULL
  })
}

#' @title Save Geoserver Data to Cache
#' @param data Object to cache
#' @param cov_name Coverage name
#' @param tier_level Tier level
#' @return Invisible NULL
#' @export
save_geoserver_cache <- function(data, cov_name, tier_level) {
  safe_name <- gsub("[:/]", "_", cov_name)
  cache_file <- paste0(DATA_PATH, "primary/cache_", safe_name, "_tier", tier_level, ".RData")

  tryCatch({
    cache_data <- data
    save(cache_data, file = cache_file)
    cli::cli_alert_success(paste0(
      "Cached geoserver data: ", cov_name, " (tier ", tier_level,
      ", rows ", nrow(data), ")"
    ))
  }, error = function(e) {
    cli::cli_alert_warning(paste0(
      "Failed to cache geoserver data for ", cov_name, ": ", conditionMessage(e)
    ))
  })
  invisible(NULL)
}

#' @title Clear Geoserver Cache
#' @param tier_level Optional tier level; NULL clears all tiers
#' @return Number of files removed
#' @export
clear_geoserver_cache <- function(tier_level = NULL) {
  pattern <- if (!is.null(tier_level)) {
    paste0("cache_.*_tier", tier_level, "\\.RData$")
  } else {
    "cache_.*_tier.*\\.RData$"
  }

  cache_files <- list.files(paste0(DATA_PATH, "primary"), pattern = pattern, full.names = TRUE)
  if (length(cache_files) == 0) return(0)

  removed <- sum(vapply(cache_files, file.remove, logical(1)))
  cli::cli_alert_success(paste0("Cleared ", removed, " geoserver cache file(s)"))
  removed
}
