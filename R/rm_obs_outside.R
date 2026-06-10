#' Remove observations outside Tier5 cells
#'
#' Filters observations to retain only those falling inside given spatial polygons.
#'
#' @title Remove observations outside spatial polygons
#' @param data.grp.tier Data frame filtered by tier with coordinates (`LONGITUDE` and `LATITUDE` columns).
#' @param HexPred_reefid2 Spatial polygons (`sf` object) defining Tier5 covariate areas.
#' @return Filtered data frame with observations inside polygons (no geometry column).
#' @author Julie Vercelloni
#' @examples
#' \dontrun{
#' library(sf)
#' # Example data frame of points
#' points_df <- data.frame(
#'   LONGITUDE = c(153.0, 153.1, 153.2),
#'   LATITUDE = c(-27.5, -27.6, -27.7),
#'   value = c(10, 20, 30)
#' )
#' 
#' # Example polygon (rectangle)
#' polygon_sf <- st_as_sf(
#'   data.frame(id = 1),
#'   wkt = "POLYGON((152.9 -27.8, 153.3 -27.8, 153.3 -27.4, 152.9 -27.4, 152.9 -27.8))",
#'   crs = 4326
#' )
#' 
#' filtered_points <- rm_obs_outside(points_df, polygon_sf)
#' }
#' @export
rm_obs_outside <- function(data.grp.tier, HexPred_reefid2, i , N) {
   status::status_try_catch(
     {
  data.grp.tier.sf <- data.grp.tier %>%
    sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

  within_check <- sf::st_within(data.grp.tier.sf, HexPred_reefid2)

  inside_indices <- which(lengths(within_check) > 0)

  data.grp.tier.filtered <- data.grp.tier.sf[inside_indices, ] %>%
    dplyr::mutate(
      LONGITUDE = sf::st_coordinates(.)[, 1],
      LATITUDE  = sf::st_coordinates(.)[, 2]
    ) %>%
    sf::st_drop_geometry()

   # Update status 
    old_item_name <- get_status_name(4, "rm_obs_outside_tier5_cells")
     if (!str_detect(old_item_name, "\\[")) {
        new_item_name = paste(old_item_name,"[",i," / ", N,"]")
     } else{
        new_item_name <- str_replace(old_item_name, "\\[([^\\]]*)\\]", paste("[",i," / ", N,"]"))
     }
    ## ##  status:::update_status_name (  # Disabled - private function not exportedstage = 4, item = "rm_obs_outside_tier5_cells", name = new_item_name)  # Disabled - private function

     },
     stage_ = 4,
     name_ = "Remove obs outside tier5 cells",
     item_ = "rm_obs_outside_tier5_cells"
   )
  
  return(data.grp.tier.filtered)
}
