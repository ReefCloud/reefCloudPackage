#' @title Tier Data Cache
#' @description Caches tier data to avoid redundant file loading operations
#' @details Uses an internal environment to store loaded tier data. Subsequent calls return cached data instead of reloading from disk.
#' @author Generated for optimization (Suggestion 29)

# Create a private environment for caching
.tier_cache <- new.env(parent = emptyenv())

#' @title Get Tier Data (Cached)
#' @description Loads tier data from file or returns cached version if already loaded
#' @param tier_level Tier level (2, 3, 4, or 5)
#' @param force_reload If TRUE, forces reload from disk even if cached
#' @return The tier.sf object for the specified tier level
#' @export
get_tier_data <- function(tier_level, force_reload = FALSE) {
  cache_key <- paste0("tier", tier_level, ".sf")

  # Check if cached and not forcing reload
  if (!force_reload && exists(cache_key, envir = .tier_cache)) {
    return(get(cache_key, envir = .tier_cache))
  }

  # Load from file
  file_path <- paste0(DATA_PATH, 'primary/tier', tier_level, '.sf.RData')

  if (!file.exists(file_path)) {
    stop(paste0("Tier data file not found: ", file_path))
  }

  # Load into temporary environment to extract the object
  temp_env <- new.env()
  load(file = file_path, envir = temp_env)

  # Verify the expected object exists in the loaded file
  if (!"tier.sf" %in% ls(temp_env)) {
    stop(paste0("tier.sf object not found in ", file_path,
                ". File may be corrupted or contain wrong object."))
  }

  tier_data <- temp_env$tier.sf

  # Additional safety check for NULL
  if (is.null(tier_data)) {
    stop(paste0("tier.sf is NULL in ", file_path,
                ". File may be empty or corrupted."))
  }

  # Cache it
  assign(cache_key, tier_data, envir = .tier_cache)

  return(tier_data)
}

#' @title Get Tiers Lookup (Cached)
#' @description Loads tiers.lookup data from file or returns cached version if already loaded
#' @param force_reload If TRUE, forces reload from disk even if cached
#' @return The tiers.lookup object
#' @export
get_tiers_lookup <- function(force_reload = FALSE) {
  cache_key <- "tiers.lookup"

  # Check if cached and not forcing reload
  if (!force_reload && exists(cache_key, envir = .tier_cache)) {
    return(get(cache_key, envir = .tier_cache))
  }

  # Load from file
  file_path <- paste0(DATA_PATH, 'primary/tiers.lookup.RData')

  if (!file.exists(file_path)) {
    stop(paste0("Tiers lookup file not found: ", file_path))
  }

  # Load into temporary environment to extract the object
  temp_env <- new.env()
  load(file = file_path, envir = temp_env)

  # Verify the expected object exists in the loaded file
  if (!"tiers.lookup" %in% ls(temp_env)) {
    stop(paste0("tiers.lookup object not found in ", file_path,
                ". File may be corrupted or contain wrong object."))
  }

  tiers_lookup <- temp_env$tiers.lookup

  # Additional safety check for NULL
  if (is.null(tiers_lookup)) {
    stop(paste0("tiers.lookup is NULL in ", file_path,
                ". File may be empty or corrupted."))
  }

  # Cache it
  assign(cache_key, tiers_lookup, envir = .tier_cache)

  return(tiers_lookup)
}

#' @title Clear Tier Cache
#' @description Clears all cached tier data
#' @export
clear_tier_cache <- function() {
  rm(list = ls(envir = .tier_cache), envir = .tier_cache)
  invisible(NULL)
}

#' @title Get Cache Status
#' @description Returns information about what's currently cached
#' @return A character vector of cached object names
#' @export
get_cache_status <- function() {
  ls(envir = .tier_cache)
}
