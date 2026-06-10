


#' @title Function
#' @description Description
#' @param parameters description
#' @return returned arguments description
#' @examples examples
#' @export
meshINLA_reconstruct <- function(GROUP, TIERS) {
  ## reefCloudPackage::ReefCloud_tryCatch({
    ## ---- reconstruct
    {
      load(paste0(DATA_PATH, "primary/tier5.sf.RData"))
      tier5.sf <- tier.sf
      load(paste0(DATA_PATH, "primary/tier4.sf.RData"))
      tier4.sf <- tier.sf
      load(paste0(DATA_PATH, "primary/tier3.sf.RData"))
      tier3.sf <- tier.sf
      load(paste0(DATA_PATH, "primary/tier2.sf.RData"))
      tier2.sf <- tier.sf

      ## ---- Level 5
      cellmeans.tier5 <- map_dfr(TIERS, .f = function(x) {
        readRDS(file = paste0(DATA_PATH, "modelled/", "cellmeans_meshSpatioTemporalTier5_",
                              DOMAIN_NAME, "_", GROUP, "_TIER", x, ".RData"))$cellmeans.tier5.sf
      })
      cellmeans <- map_dfr(TIERS, .f = function(x) {
        readRDS(file = paste0(DATA_PATH, "modelled/", "cellmeans_meshSpatioTemporalTier5_",
                              DOMAIN_NAME, "_", GROUP, "_TIER", x, ".RData"))$cellmeans.full
      })
      saveRDS(list(
        ## cellmeans.tier5 = cellmeans.tier5,
        cellmeans.tier5.sf = cellmeans.tier5.sf,
        tier5.sf = tier5.sf,
        tier4.sf = tier4.sf,
        cellmeans = cellmeans),
        file = paste0(DATA_PATH, "modelled/", "cellmeans_meshSpatioTemporalTier5_",
                      DOMAIN_NAME, "_", GROUP, ".RData"))
      ## cellmeans.tier5.sum <- map_dfr(TIERS, .f = function(x) {
      ##   get(load(file = paste0(DATA_PATH, "summarised/", "cellmeans_INLAsimpleTemporal_",
      ##                         DOMAIN_NAME, "_", GROUP, "_TIER", x, "_Tier5.RData")))
      ## })
      ## save(cellmeans.tier5.sum, file = paste0(DATA_PATH, "summarised/cellmeans_INLAsimpleTemporal_",
      ##                                     DOMAIN_NAME, "_", GROUP, "_Tier5.RData"))
      ## ----end
    }
    ## ----end
  ## }, logFile=LOG_FILE, Category='--Modelling fitting routines--',
  ## msg=paste0('Binding all simple INLA cell means for ', stringr::str_to_title(GROUP)), return=NULL)

  return(cellmeans)
}
