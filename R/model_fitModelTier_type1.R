#' @title Fit model at tier level
#' @description Fits model to data at tier level
#' @param data.grp data on which model is fitted
#' @examples model_fitModelTier()
#' @export
model_fitModelTier_type1 <- function(data.grp){
    if (reefCloudPackage::isParent()) startMatter()

    ## reefCloudPackage::ReefCloud_tryCatch({
      for (TIER in unique(data.grp[[FOCAL_TIER]])) {
        data.sub <- data.grp %>%
          filter(!!sym(FOCAL_TIER) == TIER) %>%
          droplevels()
        cellmeans(data.sub, GROUP, TIER, FOCAL_TIER)
      }
      cellmeans_reconstruct(data.grp, GROUP, TIERS = unique(data.grp[[FOCAL_TIER]]))
    ## },
    ## logFile=LOG_FILE,
    ## Category='--Modelling fitting routines--',
    ## msg='Fit simple model (Type 1)',
    ## return=NULL,
    ## stage = paste0("STAGE", CURRENT_STAGE),
    ## item = "model_type1"
    ## )
}
