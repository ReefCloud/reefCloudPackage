#' @title Fit model at tier level
#' @description Fits model to data at tier level
#' @param data.grp data on which model is fitted
#' @examples model_fitModelTier()
#' @export
model_fitModelTier_type2 <- function(data.grp){
    if (reefCloudPackage::isParent()) reefCloudPackage::startMatter()

    ## reefCloudPackage::ReefCloud_tryCatch({
      TIERS <- NULL
      for (TIER in unique(data.grp[[FOCAL_TIER]])) {
        Tier5_levels <- tiers.lookup %>%
          filter(!!sym(FOCAL_TIER) == TIER) %>%
          pull(Tier5) %>%
          unique()
        data.sub <- data.grp %>%
          filter(Tier5 %in% Tier5_levels) %>%
          ## filter(!!sym(FOCAL_TIER) == TIER) %>%
          droplevels()
        if (nrow(data.sub)>1) {
          TIERS <- c(TIERS, TIER)
          reefCloudPackage::simpleINLA(data.sub, GROUP, TIER)
        } else {
          if (!DEBUG_MODE) cli_alert_warning("Modelling for {stringr::str_to_title(GROUP)}, Tier {TIER} not performed due to a lack of data")
          reefCloudPackage::log('WARNING', logFile = LOG_FILE,
                        Category = '--Modelling (simple INLA)--',
                        msg = paste0(DOMAIN_NAME, ": ", stringr::str_to_title(GROUP),"/Tier:", TIER, "/Simple INLA reefCloudPackage::cellmeans not calculated - not enough data"))
        }
      }
      ## Gather the Tier5 posteriors together
      reefCloudPackage::simpleINLA_reconstruct(data.grp, GROUP, TIERS = TIERS)
      ## Then marginalise to levels 4, 3 and 2
      .data.tier5 <- readRDS(file = paste0(DATA_PATH, "modelled/", "cellmeans_simpleSpatioTemporalTier5_",
                                           DOMAIN_NAME, "_", GROUP, ".RData"))
      .data.tier4 <- reefCloudPackage::simpleINLA_tier4(data.sub, GROUP, .data.tier5)
      .data.tier3 <- reefCloudPackage::simpleINLA_tier3(data.sub, GROUP, .data.tier5)
      .data.tier2 <- reefCloudPackage::simpleINLA_tier2(data.sub, GROUP, .data.tier5)

    ## },
    ## logFile=LOG_FILE,
    ## Category='--Modelling fitting routines--',
    ## msg='Fit hierachical model (Type 2)',
    ## return=NULL,
    ## stage = paste0("STAGE", CURRENT_STAGE),
    ## item = "model_type2"
    ## )

    stop("end of type 2")
}
