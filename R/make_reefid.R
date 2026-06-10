#' @title Make Reef ID
#' @param tier.sf.joined A spatial layer with tier information (e.g., from `join_covariates_to_tier_lookup()`).
#' @param HexPred_sf Covariate shapefile for prediction.
#' @param reef_layer.sf A reef polygon spatial layer.
#' @return A spatial `sf` object with Tier5 and reefid columns.
#' @examples
#' \dontrun{
#' reefid_layer <- make_reefid(tier.sf.joined, HexPred_sf, reef_layer.sf)
#' }
#' @author Julie Vercelloni
#' @export
make_reefid <- function(tier.sf.joined, HexPred_sf, reef_layer.sf, i , N) {
   status::status_try_catch(
   {
  sf::sf_use_s2(TRUE) |> suppressMessages()

  # Join attribute data only (drop geometry from tier.sf.joined to avoid sf join error)
  covs.hexpred_tier_sf <- HexPred_sf |>
    dplyr::left_join(sf::st_drop_geometry(tier.sf.joined), by = c("Tier5" = "Tier5")) |>
    dplyr::filter(fYEAR == min(fYEAR))

  # Apply droplevels to attribute data only (doesn't work on geometry column)
  geom_temp <- sf::st_geometry(covs.hexpred_tier_sf)
  covs.hexpred_tier_sf <- covs.hexpred_tier_sf |>
    sf::st_drop_geometry() |>
    droplevels() |>
    sf::st_set_geometry(geom_temp) |>
    sf::st_cast("POLYGON") |>
    sf::st_transform(crs = sf::st_crs(reef_layer.sf)) |>
    suppressMessages() |>
    suppressWarnings()

  # Check CRS units
  testthat::expect_equal(sf::st_crs(covs.hexpred_tier_sf)$units, "m")
  testthat::expect_equal(sf::st_crs(reef_layer.sf)$units, "m")

  Reef_layer_tier5_84 <- reef_layer.sf |>
    sf::st_crop(covs.hexpred_tier_sf) |>
    sf::st_cast("POLYGON") |>
    dplyr::mutate(reefid = dplyr::row_number()) |>
    dplyr::select(-GRIDCODE) |>
    sf::st_transform(crs = 4326) |>
    sf::st_buffer(dist = 450) |> # careful with units and version of sf
    suppressMessages() |>
    suppressWarnings()

  covs.hexpred_tier_sf_84 <- covs.hexpred_tier_sf |>
    sf::st_transform(crs = 4326)

  # Join the shapefiles
  sf::sf_use_s2(FALSE) |> suppressMessages()

  covs.hexpred_tier_sf_v2_prep <- covs.hexpred_tier_sf_84 |>
    sf::st_join(Reef_layer_tier5_84) |>
    dplyr::select(Tier5, reefid, geometry) |>
    suppressMessages() |>
    suppressWarnings()

   # Update status 
    old_item_name <- get_status_name(4, "make_reef_id")
     if (!str_detect(old_item_name, "\\[")) {
        new_item_name = paste(old_item_name,"[",i," / ", N,"]")
     } else{
        new_item_name <- str_replace(old_item_name, "\\[([^\\]]*)\\]", paste("[",i," / ", N,"]"))
     }
    ## ##  status:::update_status_name (  # Disabled - private function not exportedstage = 4, item = "make_reef_id", name = new_item_name)  # Disabled - private function
     
   },
   stage_ = 4,
   name_ = "Make reef id",
   item_ = "make_reef_id"
   )
  return(covs.hexpred_tier_sf_v2_prep)
}
