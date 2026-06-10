#' @title cellmeans_spatiotemporal_tier3
#' @description RawMeansSpatioTemporal at Tier 3 level
#' @param data.sub data set
#' @param GROUP Benthic group
#' @param TIERS tiers considered
#' @export
cellmeans_spatiotemporal_tier3 <- function(data.sub, GROUP, TIER) {
  ## ---- RawMeansSpatioTemporalTier3
  load(paste0(DATA_PATH, "primary/tier5.sf.RData"))
  tier5.sf <- tier.sf
  load(paste0(DATA_PATH, "primary/tier4.sf.RData"))
  tier4.sf <- tier.sf
  load(paste0(DATA_PATH, "primary/tier3.sf.RData"))
  tier3.sf <- tier.sf
  cellmeans.tier3 <-
    data.sub %>%
    mutate(PERC_COVER = ifelse(!is.na(PERC_COVER), PERC_COVER, COVER)) %>%  # a single cover value
    mutate(fYEAR = factor(fYEAR, levels=(sort(levels(fYEAR))))) %>%
    group_by(fYEAR, fDEPTH, REEF, Tier3, Tier4, Tier5) %>%
    summarise(Cover=mean(PERC_COVER, na.rm=TRUE)) %>%
    ungroup %>%
    group_by(fYEAR, fDEPTH, Tier3, Tier4, Tier5) %>%
    summarise(Cover=mean(Cover, na.rm=TRUE)) %>%
    ungroup %>%
    group_by(fYEAR, fDEPTH, Tier3, Tier4) %>%
    summarise(Cover=mean(Cover, na.rm=TRUE)) %>%
    ungroup %>%
    group_by(fYEAR, fDEPTH, Tier3) %>%
    summarise(Cover=mean(Cover, na.rm=TRUE)) %>%
    ungroup %>%
    left_join(tier3.sf) %>%
    st_as_sf() %>%
    suppressMessages()
  ## Tests =========================================
  ## ggplot(cellmeans.tier3) +
  ##   geom_point(aes(y=Cover, x=as.numeric(as.character(fYEAR)), color=fDEPTH)) +
  ##   geom_line(aes(y=Cover, x=as.numeric(as.character(fYEAR)), linetype=fDEPTH)) +
  ##   facet_wrap(~Tier3)
  ## dev.off()

  ## ===============================================
  saveRDS(list(cellmeans=cellmeans.tier3,
               data = data.sub,
               tier5.sf = tier5.sf,
               tier4.sf = tier4.sf),
          file = paste0(DATA_PATH, "modelled/", "cellmeans_RawSpatioTemporal_",
                        DOMAIN_NAME, "_", GROUP, "_Tier_", TIER, "__tier3.RData"))
  ## ----end
  rm(cellmeans.tier3, tier4.sf, tier5.sf, tier.sf)
  invisible(gc(full=TRUE))
  ## cli::cli_progress_done()
}
