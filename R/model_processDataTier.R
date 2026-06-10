#' @title Process Benthic and Covariate Data at Tier Level
#' @description
#' Loads benthic monitoring data (including legacy sources), assigns each record
#' to spatial Tier levels, prepares data for modelling (e.g., by survey date and
#' benthic group), and integrates spatial covariates from the geoserver.
#' This function is typically used in workflows preparing data for either:
#' - Simple hierarchical INLA models (site-based)
#' - Full spatiotemporal FRK models (tier-based)
#' @details
#' Steps include:
#' - Retrieving and merging benthic datasets
#' - Spatially joining Tier levels (Tier 1–5)
#' - Preparing tier lookup tables
#' - Aggregating and summarising benthic photo point data by relevant groupings
#' - Preparing covariates from geospatial layers
#' @examples
#' model_processDataTier()
#' @author Murray Logan
#' @export

model_processDataTier <- function(){
  # Load legacy and current benthic data
  data <- reefCloudPackage::get_data_and_legacy_for_processing()

  # Assign spatial Tier IDs to each record
  data <- reefCloudPackage::assign_spatial_data(data)

  # Create lookup tables for Tier spatial structure
  reefCloudPackage::make_tiers_lookup()

  # Aggregate and prepare data for modelling:
  # - Summarise point data by year, depth, transect, and benthic group
  # - Compute proportional cover
  reefCloudPackage::prepare_data(data) # data <- 

  # Load and process environmental covariates (e.g., cyclones, heat stress)
  reefCloudPackage::prepare_covariates() 
}
