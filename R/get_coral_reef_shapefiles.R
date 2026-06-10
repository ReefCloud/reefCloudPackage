#' @title Get coral reef shapefiles
#' @description Unzips and loads the global coral reef shapefiles from the reefCloud package, saves them locally, and removes temporary files
#' @return NULL (saves shapefile as reef_layer.sf.RData)
#' @examples
#' get_coral_reef_shape_files()
#' @author Murray Logan
#' @export

get_coral_reef_shape_files <- function() {
  result <- status::status_try_catch(
  {
      # Check if reef_layer.sf.RData already exists
      rdata_path <- paste0(DATA_PATH, "/primary/reef_layer.sf.RData")
      if (file.exists(rdata_path)) {
        if (!DEBUG_MODE) cli::cli_alert_info("Using existing reef_layer.sf.RData")
        invisible(NULL)
      } else {
        ## Retrieve a more local version of the data
        zip_path <- system.file("extdata", package = "reefCloudPackage") %>%
          list.files(full.names = TRUE)

        if (length(zip_path) == 0) {
          stop("Coral reef shapefile not found in package. Please ensure reef_layer.sf.RData exists in ", DATA_PATH, "/primary/")
        }

        if (!DEBUG_MODE) cli_h1("Loading coral reefs of the world shapefile")
        # Use safe_unzip instead of system() (Suggestion 60)
        reefCloudPackage::safe_unzip(
          zipfile = zip_path,
          exdir = paste0(DATA_PATH, "primary/"),
          overwrite = TRUE,
          junkpaths = TRUE
        )

        reef_layer.sf <- sf::read_sf(paste0(
          DATA_PATH,
          "/primary/reef_500_poly.shp"
        ))
        save(reef_layer.sf, file = rdata_path)
        rm(reef_layer.sf)
        sf_files <- list.files(path = paste0(DATA_PATH, "/primary/"),
                               pattern = "reef_500_poly.*",
                               full.names = TRUE)
        succ <- file.remove(sf_files)
        if (any(!succ)) stop("Cannot remove old shapefiles")
        invisible(NULL)
      }
  },
  stage_ = 2,
  name_ = "Get coral reef shapefiles",
  item_ = "get_coral_reef_shapefiles"
  )
  return(result)
}
