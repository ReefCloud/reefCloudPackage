#' @title assignSpatialDomain_tier
#' @description the following functions determines which tier categories each spatial unit belongs to based on Latitude and Longitude
#' @param dat a dataframe containing at least the following:   LONGITUDE:  the Reef longitude - LATITUDE:   the Reef latitude
#' @param tier considered tier
#' @return  dataframe containing unit-level Tier classifications
#' @export
assignSpatialDomain_tier <- function(dat, tier, andNearest=TRUE) {
  load(paste0(DATA_PATH, 'primary/tier', tier, '.sf.RData'))
  dat <- dat %>%
      sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"),
               crs = st_crs(tier.sf)) %>%
      sf::st_join(tier.sf %>%
              dplyr::select(!!sym(paste0('Tier',tier))),
              join = st_intersects) %>%
      dplyr::distinct() %>%
      cbind(LONGITUDE = st_coordinates(.)[,1],
            LATITUDE = st_coordinates(.)[,2])
  ## if there are any that did not match, consider matching to the mearest feature
  if(andNearest==TRUE & any(is.na(dat[,paste0('Tier',tier)]))) {
      a <- dat %>% filter(is.na(.data[[paste0("Tier", tier)]])) %>%
          dplyr::select(-.data[[paste0("Tier", tier)]]) %>%
          sf::st_join(tier.sf %>%
                  dplyr::select(!!sym(paste0('Tier',tier))),
                  join = st_nearest_feature) %>%
          dplyr::distinct() %>%
          suppressMessages() %>%
          suppressWarnings()
      ## update the original dat according to the new a
      ## dat <- rows_update(dat, a, by=c("P_CODE", "REEF", "SITE_NO"))
      ## note, rows_update does not work nicely with sf objects (any more)
      ## So there is a need to strip the geometry of both and then put it back
      dat_no_geom <- rows_update(st_drop_geometry(dat),
                         st_drop_geometry(a),
                         by=c("P_CODE", "REEF", "SITE_NO"))
      dat <- dat_no_geom %>%
        sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), remove = FALSE, crs = st_crs(dat))
  }
  dat %>%
    sf::st_drop_geometry() %>%
    suppressMessages() %>%
    suppressWarnings()
}
