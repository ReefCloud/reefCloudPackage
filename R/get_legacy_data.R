#' @title Get legacy data
#' @description Unzip, retrieve, read, and save legacy benthic data for analysis and reporting
#' @return NULL (used for side effects: file copy, unzip, read, and save)
#' @examples
#' get_legacy_data()
#' @author Murray
#' @export

get_legacy_data <- function() {
  status::status_try_catch(
  {
    LEGACY_DATA <<- TRUE
    # Use safe_copy and safe_unzip instead of system() (Suggestion 60)
    if (DATA_FROM == "LOCAL") {
      reefCloudPackage::safe_copy(
        from = paste0(AWS_PATH, "raw/", LEGACY_FILENAME, ".zip"),
        to = paste0(DATA_PATH, "primary/", LEGACY_FILENAME, ".zip")
      )
    }
    if (INPUT_FORMAT == "zip") {
      reefCloudPackage::safe_unzip(
        zipfile = paste0(DATA_PATH, 'primary/', LEGACY_FILENAME, '.zip'),
        exdir = paste0(DATA_PATH, 'primary/'),
        overwrite = TRUE,
        junkpaths = TRUE
      )
    }
    legacy_data <- read_csv(paste0(DATA_PATH, "primary/", LEGACY_FILENAME, ".csv"),
      ## col_types = "cdccccdddddcdTcdccc",
      ## col_types = "cdcddccccdccdccd",
      col_types = "cdcddccdcdccdccd",
      trim_ws = TRUE)
    ## Convert fieldnames to uppercase (to be consistent with main data)
    legacy_data <- legacy_data %>%
      dplyr::rename_with(toupper) %>%
      mutate(SURVEY_DATE = as.POSIXct(SURVEY_DATE, format='%d/%m/%Y'),
        SITE_DEPTH = as.character(SITE_DEPTH))
    save(legacy_data, file = paste0(DATA_PATH, "primary/", gsub('reef', 'legacy', RDATA_FILE)))
    if (!DEBUG_MODE) cli_alert_success("Benthic data successfully read into: {.file {paste0(DATA_PATH, 'primary/')}}")

    if (DEBUG_MODE) reefCloudPackage::change_status(stage = paste0("STAGE", CURRENT_STAGE),
      item = "Legacy data",
      status = "success")
  },
  stage_ = 2,
  name_ = "Legacy benthic data",
  item_ = "legacy_data"
  )
}