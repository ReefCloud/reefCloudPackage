#' @title cellmeans_spatiotemporal_tier4
#' @description RawMeansSpatioTemporal at Tier 4 level
#' @param data.sub data set
#' @param GROUP Benthic group
#' @param TIERS tiers considered
#' @export
cellmeans_spatiotemporal_tier4 <- function(data.sub, GROUP, TIER) {
  ## ---- RawMeansSpatioTemporalTier4
  load(paste0(DATA_PATH, "primary/tier5.sf.RData"))
  tier5.sf <- tier.sf
  load(paste0(DATA_PATH, "primary/tier4.sf.RData"))
  tier4.sf <- tier.sf
  cellmeans.tier4 <-
    data.sub %>%
    mutate(PERC_COVER = ifelse(!is.na(PERC_COVER), PERC_COVER, COVER)) %>%  # a single cover value
    mutate(fYEAR = factor(fYEAR, levels=(sort(levels(fYEAR))))) %>%
    group_by(fYEAR, fDEPTH, REEF, Tier4, Tier5) %>%
    summarise(Cover=mean(PERC_COVER)) %>%
    ungroup %>%
    group_by(fYEAR, fDEPTH, Tier4, Tier5) %>%
    summarise(Cover=mean(Cover)) %>%
    ungroup %>%
    group_by(fYEAR, fDEPTH, Tier4) %>%
    summarise(Cover=mean(Cover)) %>%
    ungroup %>%
    left_join(tier4.sf) %>%
    st_as_sf() %>%
    suppressMessages()

  saveRDS(list(cellmeans=cellmeans.tier4,
               data = data.sub,
               tier5.sf = tier5.sf,
               tier4.sf = tier4.sf),
          file = paste0(DATA_PATH, "modelled/", "cellmeans_RawSpatioTemporal_",
                        DOMAIN_NAME, "_", GROUP, "_Tier_", TIER, "__tier4.RData"))
  rm(cellmeans.tier4, tier4.sf, tier5.sf, tier.sf)
  invisible(gc(full=TRUE))
  ## ----end
}
