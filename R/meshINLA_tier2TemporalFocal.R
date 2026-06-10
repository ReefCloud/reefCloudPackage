
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
meshINLA_tier2TemporalFocal <- function(cellmeans.full.tier2) {
  load(paste0(DATA_PATH, "primary/tier2.sf.RData"))
  tier2.sf <- tier.sf

  cellmeans.tier2.sum <-
    cellmeans.full.tier2 %>%
    left_join(tier2.sf %>%
              st_centroid() %>%
              mutate(Latitude = st_coordinates(.) %>% `[[`(1)) %>%
              dplyr::select(Tier2, Latitude) %>%
              distinct()) %>%
    group_by(fYEAR, Tier2) %>%
    mutate(Latitude = mean(Latitude)) %>%
    ungroup() %>%
    mutate(Tier2 = forcats::fct_reorder(Tier2, Latitude, min)) %>%
    group_by(fYEAR, Tier2) %>%
    mutate(value = plogis(value)) %>%
    reefCloudPackage::mean_median_hdci(value) %>%
    suppressMessages() %>%
    suppressWarnings()
  saveRDS(list(cellmeans.tier2.sum=cellmeans.tier2.sum, data = data.sub),
          file = paste0(DATA_PATH, "modelled/", "cellmeans_meshTemporalTier2_", DOMAIN_NAME, "_", GROUP, ".RData"))
  save(cellmeans.tier2.sum, file = paste0(DATA_PATH, "summarised/cellmeans_INLASPDE_", DOMAIN_NAME, "_", GROUP, "_Tier2.RData"))
}
