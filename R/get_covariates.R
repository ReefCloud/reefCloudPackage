#' @title Get Covariates
#' @description Retrieves and processes spatial covariates from the geoserver, including degree heating weeks and cyclone exposure.
#' Downloads covariates in parallel for improved performance.
#' @return NULL. Saves processed covariate data to local files.
#' @details Requires BY_TIER, DATA_PATH, and geo_info to be available in the environment. The function loads spatial data for each covariate, simplifies it, intersects it with reef tiers, and summarizes by group.
#' @author Murray Logan
#' @examples
#' get_geoserver_info()
#' get_covariates()
#' @export
get_covariates <- function() {
  COVARIATES <<- NULL
  # Use cached tier data (Suggestion 29)
  tier.sf <- reefCloudPackage::get_tier_data(as.numeric(BY_TIER))

  # Get geoserver info once and reuse (Suggestions 11, 28)
  reefCloudPackage::get_geoserver_info()

  # Cache common values (Suggestions 35, 42)
  tier_crs <- sf::st_crs(tier.sf)
  tier_valid <- sf::st_make_valid(tier.sf)
  tier_col <- paste0("Tier", BY_TIER)

  if (!DEBUG_MODE) {
    cli::cli_h2("Downloading covariate layers")
  }

  # Helper function to download and process a single covariate
  download_and_process_covariate <- function(cov_layer_name, cov_short_name) {
    if (!DEBUG_MODE) {
      cli::cli_h3(paste0("Processing ", cov_short_name))
    }

    cov_data <- reefCloudPackage::get_geoserver_data(
      Tier = as.numeric(BY_TIER) - 1,
      cov_name = cov_layer_name,
      rc_client = rc_client
    )

    if (exists("cov_data", inherits = FALSE) && !is.null(cov_data) && nrow(cov_data) > 0) {
      # Simplify geometry
      cov_data <- sf::st_simplify(cov_data, dTolerance = SPATIAL_SIMPLIFY_TOLERANCE) %>%
        suppressMessages() %>%
        suppressWarnings()

      # Ensure CRS compatibility
      if (is.na(sf::st_crs(cov_data))) {
        cov_data <- sf::st_set_crs(cov_data, SPATIAL_DEFAULT_CRS)
      }

      # Transform to tier CRS
      cov_data <- sf::st_transform(cov_data, tier_crs)

      # Intersect with tier boundaries
      cov_data <- tier_valid %>%
        sf::st_intersection(sf::st_make_valid(cov_data)) %>%
        suppressMessages() %>%
        suppressWarnings()

      return(list(success = TRUE, data = cov_data))
    } else {
      if (!DEBUG_MODE) {
        cli::cli_alert_warning(paste0("No data retrieved for ", cov_short_name))
      }
      return(list(success = FALSE, data = NULL))
    }
  }

  # Sequential download of covariates
  # Note: Parallel processing disabled due to issues with global variables and package availability in workers
  if (!DEBUG_MODE) {
    cli::cli_alert_info("Using sequential processing for covariate downloads")
  }

  ## --- Degree Heating Weeks ---
  dhw_result <- download_and_process_covariate(
    "reefcloud:degrees_heating_weeks_tier",
    "Degree Heating Weeks"
  )
  cov_dhw <- if (dhw_result$success) dhw_result$data else NULL

  ## --- Cyclones ---
  cyc_result <- download_and_process_covariate(
    "reefcloud:storm4m_exposure_year_tier",
    "Cyclone Exposure"
  )
  cov_cyc <- if (cyc_result$success) cyc_result$data else NULL

  # Process DHW data if available
  if (!is.null(cov_dhw) && nrow(cov_dhw) > 0) {
    cov_dhw <- cov_dhw %>%
      dplyr::mutate(Tier5 = as.factor(!!sym(tier_col))) %>%
      sf::st_drop_geometry() %>%
      dplyr::group_by(Tier5, year) %>%
      dplyr::summarise(
        severity_dhw = max(severity),
        max_dhw = max(dhwmax),
        end_date = max(latest)
      ) %>%
      dplyr::ungroup() %>%
      suppressMessages() %>%
      suppressWarnings()
    save(cov_dhw, file = paste0(DATA_PATH, "primary/covariate_dhw.RData"))

    if (!DEBUG_MODE) {
      cli::cli_alert_success(paste0("Processed DHW data: ", nrow(cov_dhw), " records"))
    }
  }

  # Process Cyclone data if available
  if (!is.null(cov_cyc) && nrow(cov_cyc) > 0) {
    cov_cyc <- cov_cyc %>%
      dplyr::mutate(Tier5 = as.factor(!!sym(tier_col))) %>%
      sf::st_drop_geometry() %>%
      dplyr::group_by(Tier5, end_year) %>%
      dplyr::summarise(
        severity_cyc = max(severity),
        max_cyc = max(max_hrs),
        end_date = max(end_date)
      ) %>%
      dplyr::rename(year = end_year) %>%
      dplyr::ungroup() %>%
      suppressMessages() %>%
      suppressWarnings()
    save(cov_cyc, file = paste0(DATA_PATH, "primary/covariate_cyc.RData"))

    if (!DEBUG_MODE) {
      cli::cli_alert_success(paste0("Processed cyclone data: ", nrow(cov_cyc), " records"))
    }
  }

  # Stop if both empty or NULL
  if ((is.null(cov_dhw) || nrow(cov_dhw) == 0) && (is.null(cov_cyc) || nrow(cov_cyc) == 0)) {
    stop("Disturbance layers not available from the geoserver")
  }

  rm(cov_cyc, cov_dhw, tier_valid)
  # Explicit memory cleanup after large spatial operations (Suggestion 40)
  gc()
}
