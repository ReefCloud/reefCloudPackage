
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
meshINLA_tier5Spatiotemporal <- function(tier5.sf, cellmeans.full) {
  tiers.lookup <- reefCloudPackage::get_tiers_lookup()

  cellmeans.tier5.sf <- cellmeans.full %>%
    group_by(fYEAR, Tier5) %>%
    mutate(value = plogis(value)) %>%
    summarise(mean = mean(value), median = median(value),
              CI = coda::HPDinterval(coda::as.mcmc(value))) %>%
    mutate(lower = CI[,1], upper = CI[,2]) %>%
    dplyr::select(-CI) %>%
    ## reefCloudPackage::mean_median_hdci(value) %>%
    left_join(tier5.sf) %>%
    st_as_sf() %>%
    ungroup() %>%
    suppressMessages()
  saveRDS(list(cellmeans.tier5.sf = cellmeans.tier5.sf, tier5.sf = tier5.sf,
               cellmeans.full = cellmeans.full),
          file = paste0(DATA_PATH, "modelled/", "cellmeans_meshSpatioTemporalTier5_",
                        DOMAIN_NAME, "_", GROUP, "_TIER", TIER, ".RData"))
  cellmeans.tier5.sf
}
