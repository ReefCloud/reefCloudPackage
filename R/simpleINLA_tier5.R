
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
simpleINLA_tier5 <- function(data.sub, GROUP, TIER, .data, .data.fit.cm = .data.fit.cm) {
  list2env(.data, envir = environment())
  list2env(.data.fit.cm, envir = environment())
  ## ---- simpleINLATier5Spatiotemporal
  tiers.lookup <- reefCloudPackage::get_tiers_lookup()
  load(file=paste0(DATA_PATH,'modelled/simplemod_', DOMAIN_NAME, "_", GROUP, '_TIER', TIER, '.RData'))
  load(file=paste0(DATA_PATH,'modelled/simpledraws_', DOMAIN_NAME, "_", GROUP, '_TIER', TIER, '.RData'))
  ## Posterior predictions from draws - on the whole Tier 5 grid
  cellmeans = sapply(draws, function(x) x[[2]][i.newdata.tier5])
  cellmeans = newdata.tier5 %>%
    mutate(fYEAR = factor(fYEAR, levels=fYEAR %>% unique %>% as.character() %>% sort())) %>%
    dplyr::select(fYEAR,Tier5) %>%
    cbind(plogis(cellmeans)) %>%
    rename_with(function(x)x=1, matches('plogis')) %>%
    pivot_longer(cols=matches('^[0-9]'), names_to='Rep') %>%
    ## left_join(tiers.lookup) %>%
    left_join(tiers.lookup %>% dplyr::select(Tier5, Tier4, reef_area) %>% distinct()) %>%
    suppressMessages()

  cellmeans.tier5 <- cellmeans %>%
    group_by(fYEAR, Tier4, Tier5) %>%
    reefCloudPackage::mean_median_hdci(value) %>%
    ungroup

  cellmeans.tier5.sf <-
    tier5.sf %>%
    left_join(cellmeans.tier5) %>%
    suppressMessages()

  saveRDS(list(cellmeans.tier5 = cellmeans.tier5,
               cellmeans.tier5.sf = cellmeans.tier5.sf,
               tier5.sf = tier5.sf,
               tier4.sf = tier4.sf,
               cellmeans = cellmeans),
          file = paste0(DATA_PATH, "modelled/", "cellmeans_simpleSpatioTemporalTier5_",
                        DOMAIN_NAME, "_", GROUP, "_TIER", TIER, ".RData"))
  cellmeans.tier5.sum <- cellmeans.tier5
  ## Tests =========================================
  ## cellmeans.tier5.sum %>% filter(Tier5 == '32783') %>% as.data.frame %>% head
  ## cellmeans.tier5.sum %>% filter(Tier5 == '33124') %>% as.data.frame %>% head
  ## cellmeans.tier5.sum %>% filter(Tier5 == '33124') %>% as.data.frame
  ## ===============================================

  save(cellmeans.tier5.sum, file = paste0(DATA_PATH, "summarised/cellmeans_INLAsimpleTemporal_",
                                          DOMAIN_NAME, "_", GROUP, "_TIER", TIER, "_Tier5.RData"))
  list(cellmeans.tier5 = cellmeans.tier5,
       cellmeans.tier5.sf = cellmeans.tier5.sf,
       cellmeans.tier5.sum = cellmeans.tier5.sum,
       cellmeans = cellmeans)
  ## ----end
}
