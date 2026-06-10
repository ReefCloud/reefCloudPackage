
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
simpleINLA_tier3 <- function(data.sub, GROUP, .data.tier5) {
  list2env(.data.tier5, envir = environment())
  ## ---- simpleINLATier3
  tiers.lookup <- reefCloudPackage::get_tiers_lookup()
  tier3.sf <- get(load(paste0(DATA_PATH,'primary/tier3.sf.RData')))
  ## cellmeans.tier3 <- cellmeans.tier4 %>%
  cellmeans.tier3 <- cellmeans %>%    #go back to tier 5 cellmeans
    left_join(tiers.lookup %>%
              ## group_by(Tier3, Tier4) %>%
              group_by(Tier3, Tier5) %>%
              summarise(reef_area = sum(reef_area, na.rm = TRUE))) %>%
    group_by(fYEAR, Tier3, Rep) %>%
    mutate(wt = reef_area/sum(reef_area)) %>%
    summarise(Mean = mean(value),
              value = sum(value*wt)) %>%
    ungroup() %>%
    suppressMessages()

  cellmeans.tier3.sum <- cellmeans.tier3 %>%
    group_by(fYEAR, Tier3) %>%
    reefCloudPackage::mean_median_hdci(value) %>%
    ungroup()

  cellmeans.tier3.sf <-
    tier3.sf %>%
    left_join(cellmeans.tier3.sum) %>%
    suppressMessages()

  saveRDS(list(cellmeans.tier3.sum = cellmeans.tier3.sum,
               cellmeans.tier3.sf = cellmeans.tier3.sf,
               tier3.sf = tier3.sf,
               data = data.sub),
          file = paste0(DATA_PATH, "modelled/", "cellmeans_simpleTemporalTier3_",
                        DOMAIN_NAME, "_", GROUP, ".RData"))

  save(cellmeans.tier3.sum, file = paste0(DATA_PATH, "summarised/cellmeans_INLAsimpleTemporal_",
                                          DOMAIN_NAME, "_", GROUP, "_Tier3.RData"))
  list(cellmeans = cellmeans,
       cellmeans.tier3 = cellmeans.tier3,
       cellmeans.tier3.sf = cellmeans.tier3.sf,
       cellmeans.tier3.sum = cellmeans.tier3.sum)
  ## ----end
}
