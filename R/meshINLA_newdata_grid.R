
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
meshINLA_newdata_grid <- function(data.sub, FOCAL_TIER) {
  newdata.grid <- data.sub %>%
    filter(!!sym(FOCAL_TIER) == TIER) %>%
    st_drop_geometry() %>%
    left_join(tier5.sf) %>%
    mutate(
      P_CODE = factor(P_CODE),
      Site = factor(interaction(Tier5, Site)),
      Transect = factor(interaction(Site, Transect))) %>%
    dplyr::select(P_CODE, fYEAR, fDEPTH, Site, Transect, COUNT, TOTAL, Tier5, geometry) %>%
    distinct() %>%
    st_as_sf() %>%
    st_centroid() %>%
    mutate(fYEAR = factor(fYEAR, levels = rev(sort(levels(fYEAR))))) %>%
    mutate(Longitude = st_coordinates(.)[,1],
           Latitude = st_coordinates(.)[,2]) %>%
    right_join(data.sub %>%                # there is a need for this to include all possible years in the greater data
               st_drop_geometry() %>%      # not just the ones observed in this TIER
               dplyr::select(fYEAR) %>%
               distinct()) %>%
    suppressMessages()

  newdata.coords <- newdata.grid %>%
    st_drop_geometry() %>%
    dplyr::select(Longitude,Latitude) %>%
    dplyr::filter(!is.na(Longitude)) %>%
    distinct()

  ## ---- outputs
  g <- ggplot() +
    geom_point(data=as.data.frame(newdata.coords), aes(x = Longitude, y=Latitude)) +
    theme_bw()
  ggsave(filename = paste0(OUTPUT_PATH, 'figures/mesh_newdata.grid_', GROUP, '_TIER', TIER, '.pdf'),
         g,
         width = 5, height = 5)

  g <- ggplot() +
    geom_sf(data=newdata.grid %>% dplyr::select(fYEAR) %>% distinct()) +
    facet_wrap(~fYEAR) +
    theme_bw()
  ggsave(filename = paste0(OUTPUT_PATH, 'figures/mesh_newdata.grid1_', GROUP, '_TIER', TIER, '.pdf'),
         g,
         width = 10, height = 10)
  ## ----end
  list(newdata.grid = newdata.grid, newdata.coords = newdata.coords)
}
