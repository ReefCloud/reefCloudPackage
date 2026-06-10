
## The following function generates a grid of observed points in which the
## lat/long are converted into the associated Tier5 grid
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
meshINLA_obs_grid <- function(data.sub, tier5.sf, tier5_buffer.sf) {
  data.grid <- data.sub %>%
    st_drop_geometry() %>%
    left_join(tier5.sf) %>%
    mutate(
      P_CODE = factor(P_CODE),
      Site = factor(interaction(Tier5, Site)),
      Transect = factor(interaction(Site, Transect))) %>%
    dplyr::select(P_CODE, fYEAR, fDEPTH, Site, Transect, COUNT, TOTAL, geometry) %>%
    distinct() %>%
    st_as_sf() %>%
    mutate(fYEAR = factor(fYEAR, levels = rev(sort(levels(fYEAR))))) %>%
    suppressMessages()

  coords <- data.grid %>%
    st_centroid() %>%
    st_coordinates() %>%
    `[`(,c('X','Y')) %>%
    suppressWarnings()

  ## ---- outputs
  g <- ggplot() +
    geom_sf(data = tier5_buffer.sf %>% st_centroid(), color = 'grey') +
    geom_point(data=as.data.frame(coords), aes(x = X, y=Y)) +
    theme_bw()
  ggsave(filename = paste0(OUTPUT_PATH, 'figures/mesh_obs.grid_', GROUP, '_TIER', TIER, '.pdf'),
         g,
         width = 5, height = 5)

  g <- ggplot() +
    geom_sf(data = tier5_buffer.sf %>% st_centroid(), color = 'grey') +
    geom_sf(data=data.grid %>% dplyr::select(fYEAR) %>% distinct()) +
    facet_wrap(~fYEAR) +
    theme_bw()
  ggsave(filename = paste0(OUTPUT_PATH, 'figures/mesh_obs.grid1_', GROUP, '_TIER', TIER, '.pdf'),
         g,
         width = 10, height = 10)
  ## ----end

  list(data.grid = data.grid, coords = coords)
}
