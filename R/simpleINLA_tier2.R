
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
simpleINLA_tier2 <- function(data.sub, GROUP, .data.tier5) {
  list2env(.data.tier5, envir = environment())
  ## ---- simpleINLATier2
  tiers.lookup <- reefCloudPackage::get_tiers_lookup()
  tier2.sf <- get(load(paste0(DATA_PATH,'primary/tier2.sf.RData')))
  ## cellmeans.tier2 <- cellmeans.tier4 %>%
  cellmeans.tier2 <- cellmeans %>%      #go back to tier 5 cellmeans
    left_join(tiers.lookup %>%
              ## group_by(Tier2, Tier4) %>%
              group_by(Tier2, Tier5) %>%
              summarise(reef_area = sum(reef_area, na.rm = TRUE))) %>%
    group_by(fYEAR, Tier2, Rep) %>%
    mutate(wt = reef_area/sum(reef_area)) %>%
    summarise(Mean = mean(value),
              value = sum(value*wt)) %>%
    ungroup() %>%
    suppressMessages()

  cellmeans.tier2.sum <- cellmeans.tier2 %>%
    group_by(fYEAR, Tier2) %>%
    reefCloudPackage::mean_median_hdci(value) %>%
    ungroup()

  cellmeans.tier2.sf <-
    tier2.sf %>%
    left_join(cellmeans.tier2.sum) %>%
    suppressMessages()

  saveRDS(list(cellmeans.tier2.sum = cellmeans.tier2.sum,
               cellmeans.tier2.sf = cellmeans.tier2.sf,
               tier2.sf = tier2.sf,
               data = data.sub),
          file = paste0(DATA_PATH, "modelled/", "cellmeans_simpleTemporalTier2_",
                        DOMAIN_NAME, "_", GROUP, ".RData"))

  save(cellmeans.tier2.sum, file = paste0(DATA_PATH, "summarised/cellmeans_INLAsimpleTemporal_",
                                          DOMAIN_NAME, "_", GROUP, "_Tier2.RData"))
  list(cellmeans = cellmeans,
       cellmeans.tier2 = cellmeans.tier2,
       cellmeans.tier2.sf = cellmeans.tier2.sf,
       cellmeans.tier2.sum = cellmeans.tier2.sum)
  ## ----end
}
