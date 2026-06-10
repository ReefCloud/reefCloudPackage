#' @title Generate settings
#' @description Generates path and naming settings
#' @return Creates the following global variables\cr
#' DATA_PATH\cr
#' INPUT_DATA\cr
#' RDATA_FILE \cr
#' FILENAME \cr
#' INPUT_FORMAT \cr
#' CSV_FILE \cr
#' TIER_DATA
#' @examples generateSettings()
#' @export
generateSettings <- function() {
  status::status_try_catch(
  {
  ## Location of folder to store R data objects
  DATA_PATH <<- DEFAULT_DATA_PATH
  if (!dir.exists(DATA_PATH)) dir.create(DATA_PATH)
  ## reefCloudPackage::change_status(stage = "SETTINGS", item = "DATA_PATH",
  ##   status = "success", update_display = FALSE)
  status::add_setting(element = "data_path", item = DATA_PATH, name = "Data path")

  ## Define the name of the input benthic data
  INPUT_DATA <<- "reef_data.zip"
  ## reefCloudPackage::change_status(stage = "SETTINGS", item = "INPUT_DATA",
  ##   status = "success", update_display = FALSE)
  status::add_setting(element = "input_data", item = INPUT_DATA, name = "Input data")

  ## Working name for RData version of input data
  RDATA_FILE <<- gsub('\\.csv|\\.zip','\\.RData', INPUT_DATA)
  ## reefCloudPackage::change_status(stage = "SETTINGS", item = "RDATA_FILE",
  ##   status = "success", update_display = FALSE)
  status::add_setting(element = "rdata_file", item = RDATA_FILE, name = "R data file")

  ## Working name of the data (without expension)
  FILENAME <<- gsub('\\.csv|\\.zip', '', INPUT_DATA)
  ## reefCloudPackage::change_status(stage = "SETTINGS", item = "FILENAME",
  ##   status = "success", update_display = FALSE)
  status::add_setting(element = "filname", item = FILENAME, name = "Filename")

  ## Input data format
  INPUT_FORMAT <<- ifelse(grepl('.*\\.zip$',INPUT_DATA, perl=TRUE), 'zip', 'csv')
  ## reefCloudPackage::change_status(stage = "SETTINGS", item = "INPUT_FORMAT",
  ##   status = "success", update_display = FALSE)
  status::add_setting(element = "input_format", item = INPUT_FORMAT, name = "Input format")

  CSV_FILE <<- paste0(FILENAME,".csv")
  ## reefCloudPackage::change_status(stage = "SETTINGS", item = "CSV_FILE",
  ##   status = "success", update_display = FALSE)
  status::add_setting(element = "csv_file", item = CSV_FILE, name = "CSV file")

  ## Tiers data
  TIER_DATA <<- "tiers.zip"
  ## reefCloudPackage::change_status(stage = "SETTINGS", item = "TIER_DATA",
  ##   status = "success", update_display = FALSE)
  status::add_setting(element = "tier_data", item = TIER_DATA, name = "Tier data")

  ## LOG file
  ## LOG_FILE <<- paste0(DATA_PATH, 'log/', FILENAME, '.log')
  ## LEGACY DATA
  ## LEGACY_DATA <<- FALSE
  ## reefCloudPackage::change_status(stage = "SETTINGS", item = "LEGACY_DATA",
  ##                          status = "success", update_display = FALSE)
  },
  stage_ = 1,
  name_ = "Generate settings",
  item_ = "generate_settings"
  )

}
