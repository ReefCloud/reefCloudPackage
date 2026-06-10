
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
simpleINLA_tier5a <- function(data.sub, GROUP, TIER, .data, .data.fit) {
  list2env(.data, envir = environment())
  list2env(.data.fit, envir = environment())
  ## ---- simpleINLATier5
  load(file=paste0(DATA_PATH,'modelled/simplemod_', DOMAIN_NAME, "_", GROUP, '_TIER', TIER, '.RData'))
  load(file=paste0(DATA_PATH,'modelled/simpledraws_', DOMAIN_NAME, "_", GROUP, '_TIER', TIER, '.RData'))
  fit <- mod$summary.fitted.values[i.newdata.tier5,]
  cellmeans <- newdata.tier5 %>%
    mutate(mean=fit$mean,
           lower=fit$`0.025quant`,
           upper=fit$`0.975quant`,
           median = fit$`0.5quant`) %>%
    filter(Tier5 %in% (data.sub %>% pull(Tier5) %>% unique())) %>%
    left_join(data.sub %>% dplyr::select(Tier4, Tier5) %>% distinct()) %>%
    left_join(data.sub %>%
              group_by(Tier5) %>%
              summarise(LATITUDE = mean(LATITUDE),
                        LONGITUDE = mean(LONGITUDE))) %>%
    mutate(Tier4 = forcats::fct_reorder(Tier4, LATITUDE, min)) %>%
    suppressMessages() %>%
    suppressWarnings()
  saveRDS(list(cellmeans=cellmeans, data = data.sub),
          file = paste0(DATA_PATH, "modelled/", "cellmeans_simpleTemporalTier5_",
                        DOMAIN_NAME, "_", GROUP, "_TIER", TIER, ".RData"))
  cellmeans.tier5.sum <- cellmeans
  save(cellmeans.tier5.sum, file = paste0(DATA_PATH, "summarised/cellmeans_INLAsimpleTemporal_",
                                          DOMAIN_NAME, "_", GROUP, "_TIER", TIER, "_Tier5.RData"))
  list(cellmeans = cellmeans,
       fit = fit)
  ## ----end
}
