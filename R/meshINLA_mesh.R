
## The following function creates the mesh
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
meshINLA_mesh <- function(full.grid.list, tier5_buffer.sf) {
  list2env(full.grid.list, envir = environment())

  bndry <- tier5_buffer.sf %>%
    st_buffer(0.1) %>%
    st_union() %>%
    st_convex_hull() %>%
    st_coordinates() %>%
    `[`(,c('X','Y')) %>%
    suppressMessages() %>%
    suppressWarnings()

  mesh <- inla.mesh.2d(loc = full.coords,
                       boundary = bndry,
                       max.edge = c(500, 500),
                       cutoff = 0.1, #cutoff = 0.01
                       )
  ## ---- outputs
  g <-
    ggplot() +
    inlabru::gg(mesh) +
    geom_point(data=full.coords, aes(x = Longitude, y = Latitude)) +
    ## geom_sf(data=data.grid) +
    theme_bw() +
    theme(axis.title = element_blank())

  ggsave(filename=paste0(OUTPUT_PATH, 'figures/mesh_', DOMAIN_NAME, "_",
                         str_remove_all(GROUP,' '), '_TIER', TIER, '.png'),
         g, width=5, height=6, dpi=72)
  ## ----end
  return(mesh)
}
