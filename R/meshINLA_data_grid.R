
## The following function uses the buffered FOCAL_TIER to generate a subset of
## the benthic data. The idea being that although we are focassing on a single
## FOCAL_TIER, for spatio-temporal analyses, we want to expand the training data
## a little beyond the FOCAL_TIER to ensure better spatial fitting.
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
meshINLA_data_grid <- function(data.grp, tier5.sf, tier5_buffer.list) {
  list2env(tier5_buffer.list, envir = environment())

  data.sub <- data.grp %>%
    st_as_sf(coords = c('LONGITUDE', 'LATITUDE'),
             crs = st_crs(tier5.sf),
             remove = FALSE) %>%
    st_intersection(tier5_buffer.sf)
  ## ---- outputs
  g <- ggplot() +
    geom_sf(data = tier5_buffer.sf) +
    ## geom_sf(data = tier5_sub.sf, fill = NA, color = 'blue') +
    geom_sf(data = tier5.sf %>% filter(Tier5 %in% Tier5_levels), color='red') +
    geom_sf(data = data.sub, color = 'green') +
    theme_bw()
  ggsave(filename = paste0(OUTPUT_PATH, 'figures/mesh_FocalTiers1_', GROUP, '_TIER', TIER, '.pdf'),
         g,
         width = 5, height = 5)
  ## dev.off()
  ## ----end
  return(data.sub)
}
