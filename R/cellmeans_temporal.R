#' @title cellmeans_temporal
#' @description RawMeansTemporal
#' @param data.sub data set
#' @param GROUP Benthic group
#' @param TIERS tiers considered
#' @export
cellmeans_temporal <- function(data.sub, GROUP, TIER) {
  ## ---- RawMeansTemporal
  cellmeans <-
    data.sub %>%
    mutate(PERC_COVER = ifelse(!is.na(PERC_COVER), PERC_COVER, COVER)) %>%  # a single cover value
    ## average over data type (Data and Legacy)
    group_by(fYEAR, fDEPTH, Tier4, Tier5, Site, Transect) %>%
    summarise(Cover = mean(PERC_COVER),
              LAT = mean(LATITUDE), LONG = mean(LONGITUDE)) %>%
    ungroup() %>%
    group_by(fYEAR, fDEPTH, Tier4, Tier5, Site) %>%
    summarise(Cover = mean(Cover),
              LONG = mean(LONG), LAT = mean(LAT)) %>%
    group_by(fYEAR, fDEPTH, Tier4, Tier5) %>%
    summarise(Cover = mean(Cover),
              LONG = mean(LONG), LAT = mean(LAT)) %>%
    ungroup() %>%
    mutate(Tier4 = forcats::fct_reorder(Tier4, LAT, median),
           Tier5 = forcats::fct_reorder(Tier5, LAT, median)) %>%
    suppressMessages()
  saveRDS(list(cellmeans=cellmeans, data = data.sub),
          file = paste0(DATA_PATH, "modelled/", "cellmeans_RawTemporal_", DOMAIN_NAME, "_", GROUP, "_Tier_", TIER, ".RData"))
  ## ----end
}
