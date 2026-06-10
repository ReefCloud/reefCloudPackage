#' @title cellmeans_reconstruct
#' @description Description
#' @param data.sub data set
#' @param GROUP Benthic group
#' @param TIERS tiers considered
#' @export
cellmeans_reconstruct <- function(data.sub, GROUP, TIERS) {
  ## reefCloudPackage::ReefCloud_tryCatch({
    ## ---- reconstruct
    {
      load(paste0(DATA_PATH, "primary/tier5.sf.RData"))
      tier5.sf <- tier.sf
      load(paste0(DATA_PATH, "primary/tier4.sf.RData"))
      tier4.sf <- tier.sf
      load(paste0(DATA_PATH, "primary/tier3.sf.RData"))
      tier3.sf <- tier.sf

      ## Level 5
      cellmeans <- map_dfr(TIERS, .f = function(x) {
        readRDS(file = paste0(DATA_PATH, "modelled/", "cellmeans_RawSpatioTemporal_",
                              DOMAIN_NAME, "_", GROUP, "_Tier_", x, ".RData"))$cellmeans
      })
      saveRDS(list(cellmeans=cellmeans,
                   data = data.sub,
                   tier5.sf = tier5.sf,
                   tier4.sf = tier4.sf),
              file = paste0(DATA_PATH, "modelled/", "cellmeans_RawSpatioTemporal_",
                            DOMAIN_NAME, "_", GROUP, ".RData"))

      ## Level 4
      cellmeans.tier4 <- map_dfr(TIERS, .f = function(x) {
        readRDS(file = paste0(DATA_PATH, "modelled/", "cellmeans_RawSpatioTemporal_",
                              DOMAIN_NAME, "_", GROUP, "_Tier_", x, "__tier4.RData"))$cellmeans
      })
      saveRDS(list(cellmeans=cellmeans.tier4,
                   data = data.sub,
                   tier4.sf = tier4.sf),
              file = paste0(DATA_PATH, "modelled/", "cellmeans_RawSpatioTemporal_",
                            DOMAIN_NAME, "_", GROUP, "__tier4.RData"))

      ## Level 3
      cellmeans.tier3 <- map_dfr(TIERS, .f = function(x) {
        readRDS(file = paste0(DATA_PATH, "modelled/", "cellmeans_RawSpatioTemporal_",
                              DOMAIN_NAME, "_", GROUP, "_Tier_", x, "__tier3.RData"))$cellmeans
      })
      saveRDS(list(cellmeans=cellmeans.tier3,
                   data = data.sub,
                   tier3.sf = tier3.sf),
              file = paste0(DATA_PATH, "modelled/", "cellmeans_RawSpatioTemporal_",
                            DOMAIN_NAME, "_", GROUP, "__tier3.RData"))
      rm(data.sub, cellmeans.tier3, tier4.sf, tier5.sf, tier.sf)
      invisible(gc(full=TRUE))
    }
    ## ----end
  ## }, logFile=LOG_FILE, Category='--Modelling fitting routines--',
  ## msg=paste0('Binding all raw cell means for ', stringr::str_to_title(GROUP)),
  ## return=NULL,
  ## stage = paste0("STAGE", CURRENT_STAGE),
  ## item = "bind_raw_cellmeans"
  ## )
}
