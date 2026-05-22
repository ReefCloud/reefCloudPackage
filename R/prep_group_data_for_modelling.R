#' @title Prepare grouped data for modelling
#' @description Filters monitoring data for a selected benthic group, assigns spatial tiers using spatial joins, aggregates observations at the Tier4–Tier5–year level, and formats the output for downstream modelling.
#' @param data Data frame containing monitoring observations and spatial coordinates.
#' @param GROUP Character string specifying the benthic group to filter (matching `fGROUP`).
#' @param tier.sf Spatial object containing tier polygons used for spatial assignment.
#' @return A data frame formatted for modelling, including aggregated counts, spatial tier information, coordinates, and factor variables arranged by Tier4, Tier5, Site, Transect, and descending year.
#' @examples
#' prep_group_data_for_modelling(
#'   data = monitoring_data,
#'   GROUP = "Hard coral",
#'   tier.sf = tier5_sf
#' )
#' @author Julie Vercelloni
#' @export
prep_group_data_for_modelling <- function(data, GROUP, tier.sf) {

  status::status_try_catch(
    {

      sf::sf_use_s2(FALSE)

      # Filter and spatially join data
      data.grp <- data %>%
        dplyr::filter(fGROUP == GROUP) %>%
        dplyr::select(-dplyr::any_of(c("Tier2", "Tier3", "Tier4", "Tier5"))) %>%
        sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
        sf::st_join(tier.sf) %>%
        dplyr::left_join(
          tiers.lookup %>%
            dplyr::select(-reef_area, -tier_id),
          by = "Tier5"
        ) %>%
        dplyr::filter(!is.na(Tier5))


      # Format variables for modelling
      data.grp <- data.grp %>%
        dplyr::mutate(
          dplyr::across(
            c(Tier2, Tier3, Tier4, Tier5, P_CODE),
            factor
          ),
          Site = factor(paste(Tier5, SITE_NO)),
          Transect = factor(paste(Site, TRANSECT_NO))
        ) %>%
        dplyr::arrange(
          Tier4,
          Tier5,
          Site,
          Transect,
          desc(as.numeric(as.character(fYEAR)))
        ) %>%
        dplyr::mutate(
          fYEAR = factor(fYEAR, levels = unique(fYEAR))
        ) %>%
        dplyr::mutate(
          LONGITUDE = sf::st_coordinates(geometry)[, 1],
          LATITUDE  = sf::st_coordinates(geometry)[, 2]
        ) %>%
        sf::st_drop_geometry() %>%
        droplevels()


    },
    stage_ = 4,
    order_ = 2,
    name_ = "Filter for benthic group and adjust Tier5",
    item_ = "filter_for_benthic_group"
  )

  return(data.grp)
}
