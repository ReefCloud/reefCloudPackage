
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
meshINLA_tier3TemporalFocal <- function(cellmeans.full.tier3) {
  load(paste0(DATA_PATH, "primary/tier3.sf.RData"))
  tier3.sf <- tier.sf

  cellmeans.tier3.sum <-
    cellmeans.full.tier3 %>%
    left_join(tier3.sf %>%
              st_centroid() %>%
              mutate(Latitude = st_coordinates(.) %>% `[[`(1)) %>%
              dplyr::select(Tier3, Latitude) %>%
              distinct()) %>%
    group_by(fYEAR, Tier3) %>%
    mutate(Latitude = mean(Latitude)) %>%
    ungroup() %>%
    mutate(Tier3 = forcats::fct_reorder(Tier3, Latitude, min)) %>%
    group_by(fYEAR, Tier3) %>%
    mutate(value = plogis(value)) %>%
    reefCloudPackage::mean_median_hdci(value) %>%
    suppressMessages() %>%
    suppressWarnings()
  saveRDS(list(cellmeans.tier3.sum=cellmeans.tier3.sum, data = data.sub),
          file = paste0(DATA_PATH, "modelled/", "cellmeans_meshTemporalTier3_", DOMAIN_NAME, "_", GROUP, ".RData"))
  save(cellmeans.tier3.sum, file = paste0(DATA_PATH, "summarised/cellmeans_INLASPDE_", DOMAIN_NAME, "_", GROUP, "_Tier3.RData"))
}
