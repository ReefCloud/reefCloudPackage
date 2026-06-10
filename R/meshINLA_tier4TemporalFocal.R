
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
meshINLA_tier4TemporalFocal <- function(cellmeans.full.tier4) {
  load(paste0(DATA_PATH, "primary/tier4.sf.RData"))
  tier4.sf <- tier.sf

  cellmeans.tier4.sum <-
    cellmeans.full.tier4 %>%
    left_join(tier4.sf %>%
              st_centroid() %>%
              mutate(Latitude = st_coordinates(.) %>% `[[`(1)) %>%
              dplyr::select(Tier4, Latitude) %>%
              distinct()) %>%
    group_by(fYEAR, Tier4) %>%
    mutate(Latitude = mean(Latitude)) %>%
    ungroup() %>%
    mutate(Tier4 = forcats::fct_reorder(Tier4, Latitude, min)) %>%
    group_by(fYEAR, Tier4) %>%
    mutate(value = plogis(value)) %>%
    reefCloudPackage::mean_median_hdci(value) %>%
    suppressMessages() %>%
    suppressWarnings()
  saveRDS(list(cellmeans.tier4.sum=cellmeans.tier4.sum, data = data.sub),
          file = paste0(DATA_PATH, "modelled/", "cellmeans_meshTemporalTier4_", DOMAIN_NAME, "_", GROUP, ".RData"))
  save(cellmeans.tier4.sum, file = paste0(DATA_PATH, "summarised/cellmeans_INLASPDE_", DOMAIN_NAME, "_", GROUP, "_Tier4.RData"))
}
