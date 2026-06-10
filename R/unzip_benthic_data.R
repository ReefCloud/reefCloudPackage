#' @title Unzip benthic data
#' @description Unzips the benthic data file to the primary data directory using system unzip
#' @return NULL (used for side effects: unzipping files)
#' @examples
#' unzip_benthic_data()
#' @author Murray Logan
#' @export

unzip_benthic_data <- function() {
  status::status_try_catch(
  {
    if (INPUT_FORMAT == "zip") {
      # Use safe_unzip instead of system() (Suggestion 60)
      reefCloudPackage::safe_unzip(
        zipfile = paste0(DATA_PATH, "primary/", FILENAME, ".zip"),
        exdir = paste0(DATA_PATH, "primary/"),
        overwrite = TRUE,
        junkpaths = TRUE
      )
      if (!DEBUG_MODE) {
        cli_alert_success("Benthic data successfully unzipped to: {.file {paste0(DATA_PATH, 'primary/')}}")
      }
    }
  file.remove(paste0(DATA_PATH, "primary/",FILENAME, ".zip"))
  },
  stage_ = 2,
  name_ = "Unzip benthic data",
  item_ = "unzip_benthic_data"
  )
}
