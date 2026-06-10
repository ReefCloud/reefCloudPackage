
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
meshINLA_tier4Spatiotemporal <- function(cellmeans.full.tier4) {
  load(paste0(DATA_PATH, "primary/tier4.sf.RData"))
  tier4.sf <- tier.sf

  cellmeans.tier4.sf <- cellmeans.full.tier4 %>%
    group_by(fYEAR, Tier4) %>%
    mutate(value = plogis(value)) %>%
    reefCloudPackage::mean_median_hdci(value) %>%
    left_join(tier4.sf) %>%
    st_as_sf() %>%
    ungroup() %>%
    suppressMessages()
  saveRDS(list(cellmeans.tier4.sf = cellmeans.tier4.sf, tier4.sf = tier4.sf),
          file = paste0(DATA_PATH, "modelled/", "cellmeans_meshSpatioTemporalTier4_", DOMAIN_NAME, "_", GROUP, ".RData"))
}
