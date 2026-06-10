#' @title Process data
#' @description 
#' This function is the main entry point for processing benthic data 
#' used in the modelling pipeline. It performs the following steps:
#'
#' 1. Loads the primary benthic data from an `.RData` file.
#' 2. Optionally processes legacy benthic data if available.
#' 3. Routes the data processing logic depending on whether the 
#'    analysis is targeting a site-level model (INLA) or a tier-level 
#'    model (FRK).
#' The processed datasets are saved into the `processed/` folder for use 
#' in later modelling steps. Logging and error handling are managed by 
#' the `status` package to track the data pipeline.
#' The function requires the `DATA_PATH`, `RDATA_FILE`, `DOMAIN_CATEGORY`, 
#' and optionally `LEGACY_DATA` global variables to be set. These control 
#' file paths and the type of model being run.
#' @examples 
#' model_processData()
#' @author Murray Logan
#' @export
model_processData <- function(){
  status::status_set_stage(stage = 3, title = "Process data")

  ## load in the benthic data
  status::status_try_catch(
  {
    if (!DEBUG_MODE) cli::cli_h1("Processing benthic data")
    load(file = paste0(DATA_PATH, "primary/", RDATA_FILE))
    data <- data %>%
      dplyr::rename(any_of(c("REEF" = "REEF_NAME", "REEF" = "SITE_NAME"))) %>%
      mutate(VARIABLE ="ALL")
    save(data, file=paste0(DATA_PATH, "processed/Part1_", RDATA_FILE))
   rm(data)
  },
  stage_ = 3,
  name_ = "Load benthic data",
  item_ = "load_benthic_data"
  )

  ## process the legacy benthic data (if it exists)
  if (LEGACY_DATA) {
    status::status_try_catch(
    {
      if (!DEBUG_MODE) cli::cli_h1("Processing legacy benthic data")
      load(file = paste0(DATA_PATH, "primary/", gsub('reef', 'legacy', RDATA_FILE)))
      legacy_data <- legacy_data %>%
        dplyr::rename(any_of(c("REEF" = "REEF_NAME", "REEF" = "SITE_NAME")),
          any_of(c("ID" = "ELEMENT_ID"))) %>%
        mutate(VARIABLE ="ALL")
      save(legacy_data, file=paste0(DATA_PATH, "processed/Part1_", gsub('reef', 'legacy', RDATA_FILE)))
    rm(legacy_data)
    },
    stage_ = 3,
    name_ = "Load legacy data",
    item_ = "load_legacy_data"
    )
  } else {
    ## status::remove_status_item(stage = 3, item = "load_legacy_data")  # Function removed in status package

  }

  ## There are sufficient differences in the processing of benthic data
  ## between 'site' and 'tier' level analyses to warrent forking off the
  ## code into separate paths at this point.

  if (DOMAIN_CATEGORY == "site") {
    reefCloudPackage::model_processDataSite()
  } else { # tier
    ## status::remove_status_item(stage = 3, item = "fill_data_gaps")  # Function removed in status package
    reefCloudPackage::model_processDataTier()
  }
}



