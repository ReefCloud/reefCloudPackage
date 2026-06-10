#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
simpleINLA_tier4 <- function(data.sub, GROUP, .data.tier5) {
  list2env(.data.tier5, envir = environment())
  ## ---- simpleINLATier4
  load(paste0(DATA_PATH, "primary/tier4.sf.RData"))
  tier4.sf <- tier.sf
  cellmeans.tier4 <- cellmeans %>%
    group_by(fYEAR, Tier4, Rep) %>%
    mutate(wt = reef_area/sum(reef_area)) %>%
    summarise(Mean = mean(value),
              value = sum(value*wt)) %>%
    ungroup() %>%
    suppressMessages()

  cellmeans.tier4.sum <- cellmeans.tier4 %>%
    group_by(fYEAR, Tier4) %>%
    reefCloudPackage::mean_median_hdci(value) %>%
    ungroup()

  cellmeans.tier4.sf <-
    tier4.sf %>%
    left_join(cellmeans.tier4.sum) %>%
    suppressMessages()

  saveRDS(list(cellmeans.tier4.sum = cellmeans.tier4.sum,
               cellmeans.tier4.sf = cellmeans.tier4.sf,
               tier4.sf = tier4.sf,
               data = data.sub),
          file = paste0(DATA_PATH, "modelled/", "cellmeans_simpleTemporalTier4_",
                        DOMAIN_NAME, "_", GROUP, ".RData"))
  save(cellmeans.tier4.sum, file = paste0(DATA_PATH, "summarised/cellmeans_INLAsimpleTemporal_",
                                          DOMAIN_NAME, "_", GROUP, "_Tier4.RData"))
  ## Test =========================================
  ## ggplot(cellmeans.tier4.sum) +
  ##   geom_ribbon(aes(y = mean, ymin=lower, ymax=upper, x = as.numeric(as.character(fYEAR)))) +
  ##   geom_line(aes(y = mean, x = as.numeric(as.character(fYEAR)))) +
  ##   facet_wrap(~Tier4)
  ## dev.off()
  ## ==============================================
  list(cellmeans = cellmeans,
       cellmeans.tier4 = cellmeans.tier4,
       cellmeans.tier4.sf = cellmeans.tier4.sf,
       cellmeans.tier4.sum = cellmeans.tier4.sum)
  ## ----end
}
