#' @title Cellmeans
#' @description Modelling functions (tiers) using cellmeans
#' @param data.sub data set
#' @param GROUP benthic group
#' @param TIER tier
#' @param FOCAL_TIER tier lever on which predictions are made
#' @export
cellmeans <- function(data.sub, GROUP, TIER, FOCAL_TIER) {
  ## reefCloudPackage::ReefCloud_tryCatch({
    ## ---- RawMeans
    {
      cli::cli_progress_bar("Raw cellmeans", type = "iterator", total = 4, clear = TRUE)
      reefCloudPackage::log('INFO', logFile = LOG_FILE,
                    Category = '--Modelling (Raw means)--',
                    msg = paste0(DOMAIN_NAME, ": ", GROUP, ", Tier: ", TIER, " Raw cellmeans calculated"))

      reefCloudPackage::cellmeans_temporal(data.sub, GROUP, TIER)
      cli::cli_progress_update(inc = 1, set = 1, force = TRUE)

      reefCloudPackage::cellmeans_spatiotemporal(data.sub, GROUP, TIER)
      cli::cli_progress_update(inc = 1, set = 2, force = TRUE)

      reefCloudPackage::cellmeans_spatiotemporal_tier4(data.sub, GROUP, TIER)
      cli::cli_progress_update(inc = 1, set = 3, force = TRUE)

      reefCloudPackage::cellmeans_spatiotemporal_tier3(data.sub, GROUP, TIER)
      cli::cli_progress_update(inc = 1, set = 4, force = TRUE)
    }
    ## ----end
  ## }, logFile=LOG_FILE, Category='--Modelling fitting routines--',
  ## msg=paste0('Raw cell means for ', stringr::str_to_title(GROUP)),
  ## return=NULL,
  ## stage = paste0("STAGE", CURRENT_STAGE),
  ## item = "rawCellmeans")
}
