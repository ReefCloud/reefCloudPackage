#' @title Assign Spatial Data
#' @description Assigns spatial domain (tiers) to the benthic data. First, it summarises data at the site level, then assigns spatial domains using tier shapefiles, and finally joins the spatial info back to the full dataset.
#' @param data A data frame containing benthic data including latitude and longitude.
#' @return A data frame with spatial tier classifications assigned.
#' @examples
#' data_with_spatial <- assign_spatial_data(benthic_data)
#' head(data_with_spatial)
#' @author Murray Logan
#' @export
assign_spatial_data <- function(data) {
  status::status_try_catch(
  {
    ## This only needs to be done at the site level
    data.site <- data %>%
      dplyr::group_by(P_CODE, REEF, SITE_NO) %>%
      dplyr::summarise(
        LATITUDE = mean(LATITUDE, na.rm = TRUE),
        LONGITUDE = mean(LONGITUDE, na.rm = TRUE))
    sf_use_s2(FALSE)
    data.site <-
      data.site %>%
      reefCloudPackage::assignSpatialDomain_tier(tier = 2) %>%
      reefCloudPackage::assignSpatialDomain_tier(tier = 3) %>%
      reefCloudPackage::assignSpatialDomain_tier(tier = 4) %>%
      reefCloudPackage::assignSpatialDomain_tier(tier = 5) %>%
      dplyr::select(-LONGITUDE, -LATITUDE) %>%
      dplyr::distinct() %>%
      suppressMessages() %>%
      suppressWarnings()
    data <- data %>%
      dplyr::left_join(data.site) %>%
      suppressMessages()
    rm(data.site)
    data <- data %>% dplyr::filter(!is.na(GROUP_DESC))  # this is necessary to counteract spurious joins to tiers
    if (!DEBUG_MODE) cli_alert_success("Spatial domains successfully applied to the benthic data")
  },
  stage_ = 3,
  name_ = "Assign spatial data",
  item_ = "assign_spatial_data"
  )

  return(data)
}
