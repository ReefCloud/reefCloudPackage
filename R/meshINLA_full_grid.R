

## The following function creates a full spatio-temporal grid of the data.
## Essentially, it takes the lat/longs of the tier5 centroids (full spatial
## doamin) and crosses this with the Year, Depth and Project code
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
meshINLA_full_grid <- function(data.sub, tier5_buffer.sf) {
  full.grid <- tier5_buffer.sf %>%
    st_centroid() %>%
    mutate(Longitude = st_coordinates(.)[,1],
           Latitude = st_coordinates(.)[,2]) %>%
    st_drop_geometry() %>%
    crossing(
      P_CODE = unique(data.sub$P_CODE),
      fYEAR = unique(data.sub$fYEAR),
      fDEPTH = unique(data.sub$fDEPTH)) %>%
    mutate(fYEAR = factor(fYEAR, levels = rev(sort(levels(fYEAR))))) %>%
    suppressWarnings()

  full.coords <- full.grid %>%
    dplyr::select(Longitude,Latitude) %>%
    distinct()

  ## ---- outputs
  g <- ggplot() +
    geom_point(data=full.coords, aes(x = Longitude, y=Latitude)) +
    ## geom_sf(data = tier5_buffer.sf %>% st_centroid(), color = 'green') +
    theme_bw()
  ggsave(filename = paste0(OUTPUT_PATH, 'figures/mesh_full.grid_', GROUP, '_TIER', TIER, '.pdf'),
         g,
         width = 5, height = 5)

  g <- ggplot() +
    geom_point(data=full.grid %>% dplyr::select(Longitude, Latitude, fYEAR) %>% droplevels %>% distinct(),
               aes(x = Longitude, y=Latitude)) +
    facet_wrap(~fYEAR) +
    ## geom_sf(data = tier5_buffer.sf %>% st_centroid(), color = 'green') +
    theme_bw()
  ggsave(filename = paste0(OUTPUT_PATH, 'figures/mesh_full.grid1_', GROUP, '_TIER', TIER, '.pdf'),
         g,
         width = 10, height = 10)
  ## dev.off()
  ## ----end
  list(full.grid = full.grid, full.coords = full.coords)
}
