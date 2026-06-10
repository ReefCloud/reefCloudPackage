#' @title Check packages
#' @description The following function checks to ensure that all the required packages are available on the system.
#' @param log = TRUE description
#' @return returned arguments description
#' @examples examples
#' @export
checkPackages <- function(log = TRUE) {
  status::status_try_catch(
  {
    missing <- ''
    options(tidyverse.quiet = TRUE)
    pkgs <- c(
      "tidyverse", "sf", "INLA", "jsonlite", "rlang", "tidybayes", "testthat",
      "rnaturalearth", "rnaturalearthdata", "patchwork", "ggnewscale",
      "inlabru", "cli", "stars", "geojsonR", "geojsonsf", "s2",
      "FRK", "spacetime", "ows4R", "httr"
    )
    for (p in pkgs) {
        ## unforunately we must do this the base r way until rlang is
        ## loaded
        eval(parse(text=paste0("suppressPackageStartupMessages(if(!require(",
                               p,",quietly = TRUE, warn.conflicts = FALSE)) missing <- c(missing, ",
                               p,"))")))
    }
    if (missing != "") {
      stop(paste(
        "The following required package(s) are missing: ",
        paste(missing, collapse = ", ")
      ))
    }

    ## if(missing!="") {
    ##     reefCloudPackage::log(status = "FAILURE",
    ##             logFile = LOG_FILE,
    ##             Category = "Loading the necessary R packages",
    ##             msg=NULL)
    ##     ## reefCloudPackage::change_status(stage = "STAGE1", item = "Load packages", status = "failure")
    ##     reefCloudPackage::change_status(stage = "STAGE1", item = "Load packages", status = "failure")
    ##     reefCloudPackage::openingBanner()
    ##     stop(paste('The following required package(s) are missing: ',paste(missing, collapse=', ')))
    ## } else {
    ##     ## reefCloudPackage::change_status(stage = "STAGE1", item = "Load packages", status = "success")
    ##     reefCloudPackage::change_status(stage = "STAGE1", item = "Load packages", status = "success")
    ##     reefCloudPackage::log(status = "SUCCESS",
    ##             logFile = LOG_FILE,
    ##             Category = "Loading the necessary R packages",
    ##             msg=NULL)
    ## }
  },
  stage_ = 1,
  name_ = "Load package dependencies",
  item_ = "load_packages"
  )
}
