
## The following functions are in support of model type 3 ===================================

## The following function generates a single polygon sf object that represents a
## 25% buffer around the TIERs in the FOCAL_TIER
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
meshINLA_tier_sub <- function(FOCAL_TIER, TIER, tier5.sf) {
  cli::cli_progress_bar("SPDE INLA cellmeans", type = "iterator", total = 19, clear = TRUE)


  Tier5_levels <- tiers.lookup %>%
    filter(!!sym(FOCAL_TIER) == TIER) %>%
    pull(Tier5) %>%
    unique()

  ## get the Tier 5 layer
  tier5_sub.sf <- tier5.sf %>%
    filter(Tier5 %in% Tier5_levels)
  tier5_sub.sf <- tier5_sub.sf %>%
    st_buffer(reefCloudPackage::fraction_of_bbox(st_bbox(.), frac = 0.25)) %>%
    st_union()
  tier5_buffer.sf <- tier5.sf %>% st_intersection(tier5_sub.sf)

  ## ---- outputs
  load(paste0(DATA_PATH, "primary/", stringr::str_to_lower(FOCAL_TIER), ".sf.RData"))
  tier.sf <- tier.sf %>%
    filter(!!sym(FOCAL_TIER) == TIER)
  g <- ggplot() +
    geom_sf(data = tier5_buffer.sf) +
    geom_sf(data = tier5_sub.sf, fill = NA, color = 'blue') +
    geom_sf(data = tier.sf, fill=NA, colour = "orange") +
    geom_sf(data = tier5.sf %>% filter(Tier5 %in% Tier5_levels), color='red') +
    theme_bw()
  ggsave(filename = paste0(OUTPUT_PATH, 'figures/mesh_FocalTiers_', GROUP, '_TIER', TIER, '.pdf'),
         g,
         width = 5, height = 5)
  ## dev.off()
  ## ----end
  return(list(tier5_buffer.sf = tier5_buffer.sf,
       Tier5_levels = Tier5_levels))
}
