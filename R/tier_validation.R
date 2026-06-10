#' Tier Validation Functions
#'
#' This file contains all filtering and elimination functions for tier-based
#' spatiotemporal modeling. These functions enforce data quality standards
#' before model fitting.
#'
#' @description
#' FILTERING: Functions that remove individual observations/rows
#' ELIMINATION: Functions that determine if entire tier should be excluded
#'

# ==============================================================================
# FILTERING FUNCTIONS (Remove observations, keep tier if possible)
# ==============================================================================

#' F1: Geographic Boundary Filtering
#'
#' Remove observations whose coordinates fall outside tier's polygon boundaries
#'
#' @param data.grp.tier Data frame with LONGITUDE, LATITUDE columns
#' @param spatial_polygons sf object with tier boundaries (e.g., HexPred_reefid2)
#' @param tier_name Character, name of tier (for logging)
#' @param tier_id Character/numeric, tier identifier (for logging)
#' @return List with filtered_data, n_before, n_after, pct_removed
#' @export
filter_geographic_boundaries <- function(data.grp.tier,
                                         spatial_polygons,
                                         tier_name = "Tier",
                                         tier_id = "unknown") {

  n_before <- nrow(data.grp.tier)

  # Convert observations to sf points
  data.grp.tier.sf <- data.grp.tier %>%
    sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

  # Check which points are within polygons
  within_check <- sf::st_within(data.grp.tier.sf, spatial_polygons)
  inside_indices <- which(lengths(within_check) > 0)

  # Filter to only inside points
  data.grp.tier.filtered <- data.grp.tier.sf[inside_indices, ] %>%
    dplyr::mutate(
      LONGITUDE = sf::st_coordinates(.)[, 1],
      LATITUDE  = sf::st_coordinates(.)[, 2]
    ) %>%
    sf::st_drop_geometry()

  n_after <- nrow(data.grp.tier.filtered)
  n_removed <- n_before - n_after
  pct_removed <- if (n_before > 0) (n_removed / n_before) * 100 else 0

  # Only log if observations were removed
  if (n_removed > 0) {
    cat(paste0("  Geographic filtering (", tier_name, ": ", tier_id, "): ",
               "Removed ", n_removed, " (", round(pct_removed, 2), "%) observations\n"))
  }

  return(list(
    filtered_data = data.grp.tier.filtered,
    n_before = n_before,
    n_after = n_after,
    n_removed = n_removed,
    pct_removed = pct_removed
  ))
}


#' F2: Extreme Value Quality Control (Covariates)
#'
#' Cap extreme outlier values in covariates (max_cyc, max_dhw) for prediction locations
#' that fall outside tier boundaries. Only affects prediction grid, not observation data.
#'
#' @param HexPred_sf sf object with prediction grid and covariates
#' @param data.grp.tier Data frame with tier observations (to identify data locations)
#' @param quantile_threshold Numeric, quantile threshold (default 0.975)
#' @param tier_name Character, name of tier (for logging)
#' @param tier_id Character/numeric, tier identifier (for logging)
#' @return sf object with capped covariate values
#' @export
filter_extreme_covariates <- function(HexPred_sf,
                                      data.grp.tier,
                                      quantile_threshold = 0.975,
                                      tier_name = "Tier",
                                      tier_id = "unknown") {

  # Calculate quantile thresholds
  out_cycl <- quantile(HexPred_sf$max_cyc, probs = quantile_threshold, na.rm = TRUE)
  out_dhw  <- quantile(HexPred_sf$max_dhw, probs = quantile_threshold, na.rm = TRUE)

  # Get geometry column to exclude from across() operations
  geom_col <- attr(HexPred_sf, "sf_column")
  if (is.null(geom_col)) geom_col <- "geometry"

  # Apply QC: Set to NA for prediction locations (not data locations) exceeding threshold
  HexPred_sf_filtered <- HexPred_sf %>%
    dplyr::mutate(As.Data = ifelse(Tier5 %in% data.grp.tier$Tier5, "Yes", "No")) %>%
    dplyr::mutate(across(matches("^max_cyc.*") & -all_of(geom_col),
                         ~ ifelse(.x >= out_cycl & As.Data == "No", NA, .x))) %>%
    dplyr::mutate(across(matches("^max_dhw.*") & -all_of(geom_col),
                         ~ ifelse(.x >= out_dhw & As.Data == "No", NA, .x)))

  return(HexPred_sf_filtered)
}


#' F3: Filter Site-Year Combinations with Zero Observations
#'
#' Remove site-year combinations (Tier5, fYEAR) from prediction grid that have no observations
#'
#' @param HexPred_sf sf object with prediction grid containing Tier5 and fYEAR columns
#' @param data.grp.tier Data frame with tier observations containing Tier5 and REPORT_YEAR columns
#' @param tier_name Character, name of tier (for logging)
#' @param tier_id Character/numeric, tier identifier (for logging)
#' @return List with filtered_grid, n_rows_before, n_rows_after, n_rows_removed
#' @export
filter_zero_observation_site_years <- function(HexPred_sf,
                                               data.grp.tier,
                                               tier_name = "Tier",
                                               tier_id = "unknown") {

  n_rows_before <- nrow(HexPred_sf)

  # Create site-year combinations that have observations
  # Convert REPORT_YEAR to factor matching fYEAR format
  site_years_with_data <- data.grp.tier %>%
    dplyr::mutate(fYEAR = as.factor(REPORT_YEAR)) %>%
    dplyr::distinct(Tier5, fYEAR) %>%
    dplyr::mutate(has_data = TRUE)

  # Filter prediction grid to only site-years with observations
  HexPred_sf_filtered <- HexPred_sf %>%
    dplyr::left_join(site_years_with_data, by = c("Tier5", "fYEAR")) %>%
    dplyr::filter(has_data == TRUE) %>%
    dplyr::select(-has_data)

  n_rows_after <- nrow(HexPred_sf_filtered)
  n_rows_removed <- n_rows_before - n_rows_after
  pct_removed <- if (n_rows_before > 0) (n_rows_removed / n_rows_before) * 100 else 0

  # Only log if rows were removed
  if (n_rows_removed > 0) {
    cat(paste0("  Zero-observation site-year filtering (", tier_name, ": ", tier_id, "): ",
               "Removed ", n_rows_removed, " (", round(pct_removed, 2), "%) site-year combinations\n"))
  }

  return(list(
    filtered_grid = HexPred_sf_filtered,
    n_rows_before = n_rows_before,
    n_rows_after = n_rows_after,
    n_rows_removed = n_rows_removed,
    pct_removed = pct_removed
  ))
}


# ==============================================================================
# ELIMINATION FUNCTIONS (Determine if tier should be excluded)
# ==============================================================================

#' E1: Check for Zero Observations
#'
#' Determine if tier has no observations after initial filtering
#'
#' @param data.grp.tier Data frame with tier observations
#' @param tier_name Character, name of tier
#' @param tier_id Character/numeric, tier identifier
#' @return List with valid (TRUE/FALSE) and message
#' @export
eliminate_zero_observations <- function(data.grp.tier,
                                        tier_name = "Tier",
                                        tier_id = "unknown") {

  n_obs <- nrow(data.grp.tier)

  if (n_obs == 0) {
    msg <- paste0(
      "No observations found for ", tier_name, ": ", tier_id, ". ",
      "Tier exists in hierarchy but has no associated data. ",
      "Skipping tier."
    )
    return(list(valid = FALSE, message = msg, n_obs = 0))
  }

  return(list(valid = TRUE, message = NULL, n_obs = n_obs))
}


#' E2: Check Temporal Coverage (Initial)
#'
#' Check if tier has at least minimum number of unique years before filtering
#'
#' @param data.grp.tier Data frame with REPORT_YEAR column
#' @param min_years Integer, minimum unique years required (default 2)
#' @param tier_name Character, name of tier
#' @param tier_id Character/numeric, tier identifier
#' @return List with valid (TRUE/FALSE) and message
#' @export
eliminate_insufficient_temporal_initial <- function(data.grp.tier,
                                                    min_years = 2,
                                                    tier_name = "Tier",
                                                    tier_id = "unknown") {

  n_years <- length(unique(data.grp.tier$REPORT_YEAR))

  if (n_years < min_years) {
    msg <- paste0(
      "Insufficient temporal coverage for ", tier_name, ": ", tier_id, ". ",
      "Found ", n_years, " year(s), but FRK requires >=", min_years, " years for temporal modeling. ",
      "Skipping tier."
    )
    return(list(valid = FALSE, message = msg, n_years = n_years))
  }

  return(list(valid = TRUE, message = NULL, n_years = n_years))
}


# E3: NOT IMPLEMENTED - Excessive Geographic Filtering
#
# This function is NOT USED because it doesn't make logical sense.
# If 31% of observations are outside boundaries, we should filter them out
# and continue with the remaining 69% of valid data. Geographic filtering (F1)
# already handles this correctly. Eliminating the entire tier because some
# observations are outside boundaries would throw away valid data at other sites.
#
# Kept for reference in case a reviewer wants to reconsider.
#
# #' E3: Check Excessive Geographic Filtering
# #'
# #' Determine if too many observations were removed by geographic filtering
# #'
# #' @param pct_removed Numeric, percentage of observations removed
# #' @param max_pct_removed Numeric, maximum allowed percentage (default 30)
# #' @param tier_name Character, name of tier
# #' @param tier_id Character/numeric, tier identifier
# #' @return List with valid (TRUE/FALSE) and message
# #' @export
# eliminate_excessive_filtering <- function(pct_removed,
#                                           max_pct_removed = 30,
#                                           tier_name = "Tier",
#                                           tier_id = "unknown") {
#
#   if (pct_removed > max_pct_removed) {
#     msg <- paste0(
#       round(pct_removed, 2), "% of data locations are outside Tier5 cells for ",
#       tier_name, ": ", tier_id, ". ",
#       "Maximum allowed is ", max_pct_removed, "%. ",
#       "This indicates serious spatial data quality issues. ",
#       "Skipping tier."
#     )
#     cat(paste0("  ERROR: ", msg, "\n"))
#     return(list(valid = FALSE, message = msg, pct_removed = pct_removed))
#   }
#
#   return(list(valid = TRUE, message = NULL, pct_removed = pct_removed))
# }


# E4: EXCLUDED - Minimum Observation Count is not a valid check
# Total observation count alone doesn't determine model viability.
# Example: 11 years × 2 obs/year = 22 total (passes) is worse than
#          4 years × 19 obs/year = 76 total (would fail at 20 threshold)
# Use E12 (temporal balance) and E6 (spatial coverage) instead.


#' E5: Check Temporal Coverage (Post-Filtering)
#'
#' Ensure sufficient temporal coverage remains after filtering
#'
#' @param data.grp.tier.ready Data frame after filtering with REPORT_YEAR column
#' @param min_years Integer, minimum unique years required (default 2)
#' @param tier_name Character, name of tier
#' @param tier_id Character/numeric, tier identifier
#' @return List with valid (TRUE/FALSE) and message
#' @export
eliminate_insufficient_temporal_post <- function(data.grp.tier.ready,
                                                 min_years = 2,
                                                 tier_name = "Tier",
                                                 tier_id = "unknown") {

  n_years <- length(unique(data.grp.tier.ready$REPORT_YEAR))

  if (n_years < min_years) {
    msg <- paste0(
      "Insufficient temporal coverage after filtering for ", tier_name, ": ", tier_id, ". ",
      "Found ", n_years, " year(s) after removing observations outside tier boundaries. ",
      "FRK requires at least ", min_years, " time points for temporal modeling. ",
      "Skipping tier."
    )
    return(list(valid = FALSE, message = msg, n_years = n_years))
  }

  return(list(valid = TRUE, message = NULL, n_years = n_years))
}


#' E6: Check Spatial Coverage (Post-Filtering)
#'
#' Ensure sufficient spatial coverage remains after filtering
#'
#' @param data.grp.tier.ready Data frame after filtering with Tier5 column
#' @param min_sites Integer, minimum unique sites required (default 2)
#' @param tier_name Character, name of tier
#' @param tier_id Character/numeric, tier identifier
#' @return List with valid (TRUE/FALSE) and message
#' @export
eliminate_insufficient_spatial <- function(data.grp.tier.ready,
                                           min_sites = 2,
                                           tier_name = "Tier",
                                           tier_id = "unknown") {

  n_sites <- length(unique(data.grp.tier.ready$Tier5))

  if (n_sites < min_sites) {
    msg <- paste0(
      "Insufficient spatial coverage after filtering for ", tier_name, ": ", tier_id, ". ",
      "Found ", n_sites, " site(s) (Tier5) after removing observations outside tier boundaries. ",
      "FRK requires at least ", min_sites, " spatial locations for spatial modeling. ",
      "Skipping tier."
    )
    return(list(valid = FALSE, message = msg, n_sites = n_sites))
  }

  return(list(valid = TRUE, message = NULL, n_sites = n_sites))
}


#' E7: Check Basis Function Overspecification
#'
#' Ensure ratio of basis functions to observations is not too high
#'
#' @param basis Data frame or matrix with basis functions (from FRK::auto_basis)
#' @param data.grp.tier.ready Data frame with observations after filtering
#' @param max_ratio Numeric, maximum allowed ratio (default 0.5)
#' @param tier_name Character, name of tier
#' @param tier_id Character/numeric, tier identifier
#' @return List with valid (TRUE/FALSE) and message
#' @export
eliminate_basis_overspecification <- function(basis,
                                              data.grp.tier.ready,
                                              max_ratio = 0.5,
                                              tier_name = "Tier",
                                              tier_id = "unknown") {

  n_basis <- nrow(basis)
  n_obs <- nrow(data.grp.tier.ready)
  basis_ratio <- n_basis / n_obs

  if (basis_ratio > max_ratio) {
    msg <- paste0(
      "Basis function overspecification for ", tier_name, ": ", tier_id, ". ",
      "Basis functions: ", n_basis, ", Observations: ", n_obs, ", Ratio: ", round(basis_ratio, 3), ". ",
      "Maximum allowed ratio: ", max_ratio, ". ",
      "This typically causes fitting failures due to numerical instability. ",
      "Rule of thumb: need at least 2 observations per basis function. ",
      "Skipping tier."
    )
    cat(paste0("  ERROR: ", msg, "\n"))
    return(list(valid = FALSE, message = msg, basis_ratio = basis_ratio, n_basis = n_basis, n_obs = n_obs))
  }

  return(list(valid = TRUE, message = NULL, basis_ratio = basis_ratio, n_basis = n_basis, n_obs = n_obs))
}


#' E8: Check for Duplicate (Tier5, fYEAR) Keys in Prediction Grid
#'
#' Ensure prediction grid has no duplicate site-year combinations
#'
#' @param HexPred_sp Spatial object with prediction grid
#' @param tier_name Character, name of tier
#' @param tier_id Character/numeric, tier identifier
#' @return List with valid (TRUE/FALSE) and message
#' @export
eliminate_duplicate_hexpred_keys <- function(HexPred_sp,
                                             tier_name = "Tier",
                                             tier_id = "unknown") {

  # Check for duplicate (Tier5, fYEAR) combinations
  dup_check <- tryCatch({
    as.data.frame(HexPred_sp@data) %>%
      dplyr::group_by(Tier5, fYEAR) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      dplyr::filter(n > 1)
  }, error = function(e) {
    msg <- paste0("Duplicate check failed for ", tier_name, ": ", tier_id, ": ", e$message)
    cat(paste0("  ERROR: ", msg, "\n"))
    return(data.frame(Tier5 = character(), fYEAR = character(), n = integer()))
  })

  if (nrow(dup_check) > 0) {
    # Show first few duplicates
    dup_examples <- head(dup_check, 3)
    dup_str <- paste(apply(dup_examples, 1, function(x) paste0(x[1], "/", x[2], " (", x[3], "x)")), collapse = ", ")

    msg <- paste0(
      "Duplicate (Tier5, fYEAR) keys in prediction grid for ", tier_name, ": ", tier_id, ". ",
      "Found ", nrow(dup_check), " duplicate site-year combinations. ",
      "Expected: 1 row per hexagon per year. ",
      "Examples: ", dup_str, if (nrow(dup_check) > 3) paste0(" and ", nrow(dup_check) - 3, " more") else "", ". ",
      "This will cause dimension mismatch in FRK::auto_basis(). ",
      "Skipping tier."
    )
    cat(paste0("  ERROR: ", msg, "\n"))
    return(list(valid = FALSE, message = msg, n_duplicates = nrow(dup_check), duplicates = dup_check))
  }

  return(list(valid = TRUE, message = NULL, n_duplicates = 0))
}


#' E9: Check HexPred Dimension Consistency
#'
#' Ensure prediction grid dimensions match expected hexagons × years
#'
#' @param HexPred_sp Spatial object with prediction grid
#' @param tier_name Character, name of tier
#' @param tier_id Character/numeric, tier identifier
#' @return List with valid (TRUE/FALSE) and message
#' @export
eliminate_hexpred_dimension_mismatch <- function(HexPred_sp,
                                                 tier_name = "Tier",
                                                 tier_id = "unknown") {

  unique_years <- sort(unique(HexPred_sp@data$fYEAR))
  nYEAR <- length(unique_years)
  nHEX <- nrow(subset(HexPred_sp, fYEAR == min(HexPred_sp@data$fYEAR)))

  expected_rows <- nHEX * nYEAR
  actual_rows <- nrow(HexPred_sp@data)

  if (actual_rows != expected_rows) {
    pct_coverage <- round(actual_rows / expected_rows * 100, 1)
    msg <- paste0(
      "Prediction grid dimension mismatch for ", tier_name, ": ", tier_id, ". ",
      "Expected: ", nHEX, " hexagons × ", nYEAR, " years = ", expected_rows, " rows. ",
      "Actual: ", actual_rows, " rows (", pct_coverage, "% coverage). ",
      "Missing ", expected_rows - actual_rows, " site-year combinations. ",
      "This indicates incomplete spatial-temporal grid that may cause FRK to fail. ",
      "Skipping tier."
    )
    cat(paste0("  ERROR: ", msg, "\n"))
    return(list(
      valid = FALSE,
      message = msg,
      expected_rows = expected_rows,
      actual_rows = actual_rows,
      n_hexagons = nHEX,
      n_years = nYEAR
    ))
  }

  return(list(
    valid = TRUE,
    message = NULL,
    expected_rows = expected_rows,
    actual_rows = actual_rows,
    n_hexagons = nHEX,
    n_years = nYEAR
  ))
}


#' E10: Check BAU Structure Consistency
#'
#' Ensure Basic Areal Unit grid dimensions match expected spatial × temporal dimensions
#'
#' @param ST_BAUs Spatial-temporal BAU object
#' @param n_spatial Integer, number of spatial units
#' @param n_temporal Integer, number of temporal units
#' @param tier_name Character, name of tier
#' @param tier_id Character/numeric, tier identifier
#' @return List with valid (TRUE/FALSE) and message
#' @export
eliminate_bau_corruption <- function(ST_BAUs,
                                     n_spatial,
                                     n_temporal,
                                     tier_name = "Tier",
                                     tier_id = "unknown") {

  expected_rows <- n_spatial * n_temporal
  actual_rows <- nrow(ST_BAUs@data)

  if (actual_rows != expected_rows) {
    msg <- paste0(
      "BAU structure corruption for ", tier_name, ": ", tier_id, ". ",
      "Expected: ", n_spatial, " spatial × ", n_temporal, " temporal = ", expected_rows, " rows. ",
      "Actual: ", actual_rows, " rows in ST_BAUs@data. ",
      "This indicates corrupted Basic Areal Unit structure. ",
      "Cannot assign covariates or proceed with FRK. ",
      "Skipping tier."
    )
    cat(paste0("  ERROR: ", msg, "\n"))
    return(list(
      valid = FALSE,
      message = msg,
      expected_rows = expected_rows,
      actual_rows = actual_rows,
      n_spatial = n_spatial,
      n_temporal = n_temporal
    ))
  }

  return(list(
    valid = TRUE,
    message = NULL,
    expected_rows = expected_rows,
    actual_rows = actual_rows,
    n_spatial = n_spatial,
    n_temporal = n_temporal
  ))
}


#' E11: Check for BAU-HexPred Join Row Explosion
#'
#' Ensure left join between BAUs and HexPred doesn't increase row count
#'
#' @param bau_data_before Data frame, BAU data before join
#' @param bau_data_after Data frame, BAU data after join
#' @param tier_name Character, name of tier
#' @param tier_id Character/numeric, tier identifier
#' @return List with valid (TRUE/FALSE) and message
#' @export
eliminate_bau_join_explosion <- function(bau_data_before,
                                         bau_data_after,
                                         tier_name = "Tier",
                                         tier_id = "unknown") {

  rows_before <- nrow(bau_data_before)
  rows_after <- nrow(bau_data_after)

  if (rows_after != rows_before) {
    explosion_factor <- round(rows_after / rows_before, 2)
    msg <- paste0(
      "BAU-HexPred join row explosion for ", tier_name, ": ", tier_id, ". ",
      "Before join: ", rows_before, " rows. ",
      "After join: ", rows_after, " rows (", explosion_factor, "x). ",
      "This indicates duplicate keys in HexPred causing cartesian product. ",
      "FRK::auto_basis() will fail with 'data length not multiple of columns' error. ",
      "Skipping tier."
    )
    cat(paste0("  ERROR: ", msg, "\n"))
    return(list(
      valid = FALSE,
      message = msg,
      rows_before = rows_before,
      rows_after = rows_after,
      explosion_factor = explosion_factor
    ))
  }

  return(list(
    valid = TRUE,
    message = NULL,
    rows_before = rows_before,
    rows_after = rows_after
  ))
}


#' E12: Check Minimum Observations AND Sites Per Year
#'
#' Ensure each year has sufficient observations AND sufficient sites for spatial modeling
#'
#' @param data.grp.tier.ready Data frame after filtering with REPORT_YEAR and Tier5 columns
#' @param min_obs_per_year Integer, minimum observations per year (default 3)
#' @param min_sites_per_year Integer, minimum sites per year (default 2)
#' @param tier_name Character, name of tier
#' @param tier_id Character/numeric, tier identifier
#' @return List with valid (TRUE/FALSE) and message
#' @export
eliminate_insufficient_obs_per_year <- function(data.grp.tier.ready,
                                                min_obs_per_year = 3,
                                                min_sites_per_year = 2,
                                                tier_name = "Tier",
                                                tier_id = "unknown") {

  # Calculate observations per year
  obs_per_year <- table(data.grp.tier.ready$REPORT_YEAR)
  min_year_obs <- min(obs_per_year)

  # Calculate sites per year with detailed breakdown
  year_details <- data.grp.tier.ready %>%
    dplyr::group_by(REPORT_YEAR) %>%
    dplyr::summarise(
      n_obs = dplyr::n(),
      n_sites = length(unique(Tier5)),
      sites = paste(unique(Tier5), collapse = ", "),
      .groups = "drop"
    )

  min_year_sites <- min(year_details$n_sites)

  # Find problematic years
  problem_years <- year_details %>%
    dplyr::filter(n_obs < min_obs_per_year | n_sites < min_sites_per_year)

  # Only log if there are failures
  if (nrow(problem_years) > 0) {
    msg <- paste0(
      "Insufficient observations or spatial coverage per year for ", tier_name, ": ", tier_id, ". ",
      "FRK requires at least ", min_obs_per_year, " observations and ", min_sites_per_year, " sites per year for spatial modeling.\n"
    )

    # Add detailed breakdown for each problem year
    for (i in 1:nrow(problem_years)) {
      yr <- problem_years$REPORT_YEAR[i]
      n_obs <- problem_years$n_obs[i]
      n_sites <- problem_years$n_sites[i]
      sites <- problem_years$sites[i]

      msg <- paste0(msg,
        "    Year ", yr, ": ", n_obs, " observation(s) at ", n_sites, " site(s) [", sites, "]",
        if (n_obs < min_obs_per_year) paste0(" - INSUFFICIENT OBS (need ", min_obs_per_year, ")") else "",
        if (n_sites < min_sites_per_year) paste0(" - INSUFFICIENT SITES (need ", min_sites_per_year, ")") else "",
        "\n"
      )
    }

    msg <- paste0(msg, "  Skipping tier.")

    cat(paste0("  ERROR: ", msg, "\n"))
    return(list(
      valid = FALSE,
      message = msg,
      min_obs_per_year = min_year_obs,
      min_sites_per_year = min_year_sites,
      obs_per_year = as.list(obs_per_year),
      year_details = year_details
    ))
  }

  return(list(
    valid = TRUE,
    message = NULL,
    min_obs_per_year = min_year_obs,
    min_sites_per_year = min_year_sites,
    obs_per_year = as.list(obs_per_year),
    year_details = year_details
  ))
}



# ==============================================================================
# COMPOSITE VALIDATION FUNCTION
# ==============================================================================

#' Validate Tier After All Filtering
#'
#' Apply all post-filtering elimination checks in sequence
#'
#' @param data.grp.tier.ready Data frame after filtering
#' @param tier_name Character, name of tier
#' @param tier_id Character/numeric, tier identifier
#' @param min_years Integer, minimum years (default 2)
#' @param min_sites Integer, minimum sites (default 2)
#' @param min_obs_per_year Integer, minimum observations per year (default 3)
#' @param min_sites_per_year Integer, minimum sites per year (default 2)
#' @return List with valid (TRUE/FALSE), failed_check, and message
#' @export
validate_tier_post_filtering <- function(data.grp.tier.ready,
                                         tier_name = "Tier",
                                         tier_id = "unknown",
                                         min_years = 2,
                                         min_sites = 2,
                                         min_obs_per_year = 3,
                                         min_sites_per_year = 2) {

  # E5: Temporal coverage (minimum years)
  check_e5 <- eliminate_insufficient_temporal_post(data.grp.tier.ready, min_years, tier_name, tier_id)
  if (!check_e5$valid) {
    return(list(valid = FALSE, failed_check = "E5", message = check_e5$message))
  }

  # E6: Spatial coverage (minimum sites)
  check_e6 <- eliminate_insufficient_spatial(data.grp.tier.ready, min_sites, tier_name, tier_id)
  if (!check_e6$valid) {
    return(list(valid = FALSE, failed_check = "E6", message = check_e6$message))
  }

  # E12: Temporal balance (observations AND sites per year)
  check_e12 <- eliminate_insufficient_obs_per_year(data.grp.tier.ready, min_obs_per_year, min_sites_per_year, tier_name, tier_id)
  if (!check_e12$valid) {
    return(list(valid = FALSE, failed_check = "E12", message = check_e12$message))
  }

  # All checks passed - no success logging to keep logs small
  return(list(valid = TRUE, failed_check = NULL, message = NULL))
}
