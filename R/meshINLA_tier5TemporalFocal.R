

#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
meshINLA_tier5TemporalFocal <- function(data.sub, cellmeans.full, FOCAL_TIER) {
  tiers.lookup <- reefCloudPackage::get_tiers_lookup()

  ## Restrict this to just the observed (focal)Tier5 hexagons, otherwise the grid of plots will be huge!
  cellmeans.tier5 <- cellmeans.full %>%
    right_join(data.sub %>%
               filter(!!sym(FOCAL_TIER) == TIER) %>%
               st_drop_geometry() %>%
               dplyr::select(Tier5, Tier4) %>%
               distinct()) %>%
    group_by(fYEAR, Tier4, Tier5) %>%
    mutate(Latitude = mean(Latitude)) %>%
    ungroup() %>%
    mutate(Tier4 = forcats::fct_reorder(Tier4, Latitude, min),
           Tier5 = forcats::fct_reorder(Tier5, Latitude, min)) %>%
    group_by(fYEAR, Tier4, Tier5) %>%
    mutate(value = plogis(value)) %>%
    reefCloudPackage::mean_median_hdci(value) %>%
    suppressMessages()
  saveRDS(list(cellmeans.tier5=cellmeans.tier5, data = data.sub),
          file = paste0(DATA_PATH, "modelled/", "cellmeans_meshTemporalTier5focal_", DOMAIN_NAME, "_",
                        GROUP, "_TIER", TIER, ".RData"))
}
