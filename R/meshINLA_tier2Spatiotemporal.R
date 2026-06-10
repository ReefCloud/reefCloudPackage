
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
meshINLA_tier2Spatiotemporal <- function(cellmeans.full.tier2) {
  load(paste0(DATA_PATH, "primary/tier2.sf.RData"))
  tier2.sf <- tier.sf

  cellmeans.tier2.sf <- cellmeans.full.tier2 %>%
    group_by(fYEAR, Tier2) %>%
    mutate(value = plogis(value)) %>%
    reefCloudPackage::mean_median_hdci(value) %>%
    left_join(tier2.sf) %>%
    st_as_sf() %>%
    ungroup() %>%
    suppressMessages()
  saveRDS(list(cellmeans.tier2.sf = cellmeans.tier2.sf, tier2.sf = tier2.sf),
          file = paste0(DATA_PATH, "modelled/", "cellmeans_meshSpatioTemporalTier2_", DOMAIN_NAME, "_", GROUP, ".RData"))
}
