
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
meshINLA_tier2 <- function(cellmeans.full) {
  tier2.sf <- get(load(paste0(DATA_PATH,'primary/tier2.sf.RData')))
  tiers.lookup <- reefCloudPackage::get_tiers_lookup()

  cellmeans.full.tier2 <- cellmeans.full %>%
    left_join(tiers.lookup %>%
              group_by(Tier2, Tier5) %>%
              summarise(reef_area = sum(reef_area, na.rm = TRUE))) %>%
    group_by(Tier2, fYEAR, Rep) %>%
    mutate(wt = reef_area / sum(reef_area)) %>%
    summarise(value = sum(value * wt)) %>%
    ungroup() %>%
    suppressMessages()
  return(cellmeans.full.tier2)
}
