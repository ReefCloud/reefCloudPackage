

#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
meshINLA_tier3 <- function(cellmeans.full) {
  tier3.sf <- get(load(paste0(DATA_PATH,'primary/tier3.sf.RData')))
  tiers.lookup <- reefCloudPackage::get_tiers_lookup()

  cellmeans.full.tier3 <- cellmeans.full %>%
    left_join(tiers.lookup %>%
              group_by(Tier3, Tier5) %>%
              summarise(reef_area = sum(reef_area, na.rm = TRUE))) %>%
    group_by(Tier3, fYEAR, Rep) %>%
    mutate(wt = reef_area / sum(reef_area)) %>%
    summarise(value = sum(value * wt)) %>%
    ungroup() %>%
    suppressMessages()
  return(cellmeans.full.tier3)
}
