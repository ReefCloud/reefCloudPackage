#' @title Fit model at tier level
#' @description Fits model to data at tier level
#' @param data.grp data on which model is fitted
#' @examples model_fitModelTier()
#' @export
model_fitModelTier_type4 <- function(data.grp){
  if (reefCloudPackage::isParent()) reefCloudPackage::startMatter()

  ## reefCloudPackage::ReefCloud_tryCatch({

    tiers.lookup <<- reefCloudPackage::get_tiers_lookup()
    tier5.sf <- reefCloudPackage::get_tier_data(5)

    ## reefCloudPackage::ReefCloud_tryCatch({
      TIERS <- NULL
      for (TIER in unique(data.grp[[FOCAL_TIER]])) {
        TIER <<- TIER  #make this global
        ## Subset the FOCAL TIER with a buffer
        tier5_buffer.list <- reefCloudPackage::meshINLA_tier_sub(FOCAL_TIER, TIER, tier5.sf)
        ## Subset the data
        data.sub <- reefCloudPackage::meshINLA_data_grid(data.grp, tier5.sf, tier5_buffer.list)
        if(nrow(data.sub) >1) {
          TIERS <- c(TIERS, TIER)
          reefCloudPackage::meshINLA(data.sub, tier5.sf, tier5_buffer.list, FOCAL_TIER)
        } else {
          if (!DEBUG_MODE) cli_alert_warning("Modelling for {stringr::str_to_title(GROUP)}, Tier {TIER} not performed due to a lack of data")
          reefCloudPackage::log('WARNING', logFile = LOG_FILE,
                        Category = '--Modelling (mesh INLA)--',
                        msg = paste0(DOMAIN_NAME, ": ", stringr::str_to_title(GROUP),"/Tier:", TIER, "/Mesh INLA cellmeans not calculated - not enough data"))
        }
      }
    ## }, logFile=LOG_FILE, Category='--Modelling fitting routines--',
    ## msg=paste0('Mesh (with covariates) INLA cell means for ', stringr::str_to_title(GROUP))
    ## )

  ## },
  ## logFile=LOG_FILE,
  ## Category='--Modelling fitting routines--',
  ## msg='Fit simple model (Type 2)',
  ## return=NULL,
  ## stage = paste0("STAGE", CURRENT_STAGE),
  ## item = "model_type2"
  ## )
}
