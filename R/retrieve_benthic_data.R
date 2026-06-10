#' @title Retrieve benthic data
#' @description Retrieve the benthic data from the source specified by `AWS_PATH` and `FILENAME`, and save it to the `DATA_PATH` directory
#' @return NULL (used for side effects: copying or downloading files)
#' @examples
#' retrieve_benthic_data()
#' @author Murray Logan
#' @export

retrieve_benthic_data <- function() {
  status::status_try_catch(
  {
    ## Retrieve a more local version of the data
    if (!DEBUG_MODE) cli_h1("Loading data")
    if (DATA_FROM == "S3") {
      reefCloudPackage::load_aws(file = CSV_FILE, level = "primary/")
    }
    # Use safe_copy instead of system() for all data sources (Suggestion 60)
    if (DATA_FROM == "LOCAL") {
      reefCloudPackage::safe_copy(
        from = paste0(AWS_PATH, "raw/", FILENAME, ".zip"),
        to = paste0(DATA_PATH, "primary/", FILENAME, ".zip")
      )
    }
    if (DATA_FROM == "SYNTHETIC") {
      reefCloudPackage::safe_copy(
        from = paste0(AWS_PATH, "raw/", FILENAME, ".zip"),
        to = paste0(DATA_PATH, "primary/", FILENAME, ".zip")
      )
    }
    if (DATA_FROM == "User defined") {
      reefCloudPackage::safe_copy(
        from = paste0(AWS_PATH, "raw/", FILENAME, ".zip"),
        to = paste0(DATA_PATH, "primary/", FILENAME, ".zip")
      )
    }
  },
  stage_ = 2,
  name_ = "Retrieve benthic data",
  item_ = "retrieve_benthic_data"
  )
  return(NULL)
}
