
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
meshINLA_tier3Spatiotemporal <- function(cellmeans.full.tier3) {
  load(paste0(DATA_PATH, "primary/tier3.sf.RData"))
  tier3.sf <- tier.sf

  cellmeans.tier3.sf <- cellmeans.full.tier3 %>%
    group_by(fYEAR, Tier3) %>%
    mutate(value = plogis(value)) %>%
    reefCloudPackage::mean_median_hdci(value) %>%
    left_join(tier3.sf) %>%
    st_as_sf() %>%
    ungroup() %>%
    suppressMessages()
  saveRDS(list(cellmeans.tier3.sf = cellmeans.tier3.sf, tier3.sf = tier3.sf),
          file = paste0(DATA_PATH, "modelled/", "cellmeans_meshSpatioTemporalTier3_", DOMAIN_NAME, "_", GROUP, ".RData"))
}
