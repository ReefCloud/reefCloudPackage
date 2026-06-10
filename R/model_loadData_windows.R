#' @title Load data
#' @description This script will copy all the data sources from the bucket provided and place in <DATA_PATH>/primary.\cr
#' When run on the dev server (HPC) DATA_FROM will be LOCAL\cr
#' When run on the prod server (AWS) DATA_FROM will be S3\cr
#' There should be three data collections in <AWS_PATH>/raw:\cr
#' - benthic data (reefs_data.csv|zip)\cr
#' - spatial tiers data (tiers[2-5].shp)\cr
#' - covariates data (?)
#' @examples model_loadData_windows()
##' @export
model_loadData_windows <- function(){
  if (reefCloudPackage::isParent()) reefCloudPackage::startMatter()

  reefCloudPackage::read_status()
  CURRENT_STAGE <<- 2

  ## Benthic data =============================================================
  ## 1. Retrieve the benthic data from the S3 bucket
  ## reefCloudPackage::ReefCloud_tryCatch({
    ## Retrieve a more local version of the data
    if (!DEBUG_MODE) cli_h1("Loading data")
    if (DATA_FROM == "S3") reefCloudPackage::load_aws(file = CSV_FILE, level = "primary/")
    if (DATA_FROM == "LOCAL") {
      source_file <- paste0(AWS_PATH, "raw/", FILENAME, ".zip")
      destination_file <- paste0(DATA_PATH, "primary/", FILENAME, ".zip")
      file.copy(from = source_file, to = destination_file, overwrite = TRUE)
    }
    if (DATA_FROM == "SYNTHETIC"){
      source_file <- paste0(AWS_PATH, "raw/", FILENAME, ".zip")
      destination_file <- paste0(DATA_PATH, "primary/", FILENAME, ".zip")
      file.copy(from = source_file, to = destination_file, overwrite = TRUE)
    }
    if (DATA_FROM == "User defined") {
      source_file <- paste0(AWS_PATH, "raw/", FILENAME, ".zip")
      destination_file <- paste0(DATA_PATH, "primary/", FILENAME, ".zip")
      file.copy(from = source_file, to = destination_file, overwrite = TRUE)
      }
  ## },
  ## logFile = LOG_FILE,
  ## Category = "--Data processing routines--",
  ## msg = "Retrieve benthic data from supplied bucket",
  ## stage = paste0("STAGE", CURRENT_STAGE),
  ## item = "Retrieve data")

  ## 2. Unzip data
  ## reefCloudPackage::ReefCloud_tryCatch({
    if (INPUT_FORMAT == "zip") {
      zip_file <- paste0(DATA_PATH, 'primary/', FILENAME, '.zip')
      extract_dir <- paste0(DATA_PATH, 'primary/')
      unzip(zipfile = zip_file, exdir = extract_dir)
      if (!DEBUG_MODE) cli::cli_alert_success("Benthic data successfully unzipped to: {.file {paste0(DATA_PATH, 'primary/')}}")
    }
  ## },
  ## logFile = LOG_FILE,
  ## Category = "--Data processing routines--",
  ## msg = "Unzip benthic data",
  ## stage = paste0("STAGE", CURRENT_STAGE),
  ## item = "Unzip data")

  ## 3. Import data
  ## reefCloudPackage::ReefCloud_tryCatch({
    ## Read data into a R
    data <- read_csv(paste0(DATA_PATH, "primary/", CSV_FILE),
                     ## col_types = "cdccccdddddcdTcdccc",
                     ## col_types = "cdcddccccdTcdcc",
                     col_types = "cdcddccdcdTcdcc",
                     trim_ws = TRUE)

    names(data) <- c("P_CODE", "ID", "SITE_NAME",
                     "LATITUDE",	"LONGITUDE", "SITE_NO",
                     "TRANSECT_NO",	"SITE_DEPTH",	"REEF_ZONE",
                     "REPORT_YEAR",	"SURVEY_DATE",	"FRAME"	,
                     "POINT_NO",	"GROUP_DESC",	"BENTHIC_CATEGORY")
    data <- data %>%
      mutate(SITE_DEPTH = as.character(SITE_DEPTH))
  ## },
  ## logFile = LOG_FILE,
  ## Category = "--Data processing routines--",
  ## msg = "Importing benthic data from local store",
  ## stage = paste0("STAGE", CURRENT_STAGE),
  ## item = "Import data"
  ## )

  ## 3. Save data
  ## reefCloudPackage::ReefCloud_tryCatch({
    ## Save as native R file
    save(data, file = paste0(DATA_PATH, "primary/", RDATA_FILE))
    if (!DEBUG_MODE) cli::cli_alert_success("Benthic data successfully read into: {.file {paste0(DATA_PATH, 'primary/')}}")
    if (GENERATE_REPORT) {
      ANALYSIS_STAGE <<- c(ANALYSIS_STAGE,
                           list(list(type='component', value = '31a_load_benthos'))) %>%
        unique()
      save(ANALYSIS_STAGE, file=paste0(DATA_PATH, "analysis_stage.RData"))
      ## if(!reefCloudPackage::build_report(component = "load_benthos"))
      ##     cli_alert_danger("Info on loaded benthic data is {col_red(style_bold('NOT'))} incorporated into report!")
    }
  ## },
  ## logFile = LOG_FILE,
  ## Category = "--Data processing routines--",
  ## msg = "Saving benthic data",
  ## stage = paste0("STAGE", CURRENT_STAGE),
  ## item = "Save data"
  ## )

  ## Retrieve legacy benthic data (if it exists) ===================================================
  ## The legacy data may have an additional field ('cover') that records
  ## the percentage cover of the benthos.  If this field is non-NA,
  ## then the 'frame' and 'point_no' fields will be NA
  LEGACY_FILENAME <<- 'legacy_data'
  LEGACY_DATA <<- FALSE
  if(file.exists(paste0(AWS_PATH, 'raw/', LEGACY_FILENAME, '.zip'))) {
    ## reefCloudPackage::ReefCloud_tryCatch({
      LEGACY_DATA <<- TRUE
      if (DATA_FROM == "LOCAL"){
        source_file <- paste0(AWS_PATH, "raw/", LEGACY_FILENAME, ".zip")
        destination_file <- paste0(DATA_PATH, "primary/", LEGACY_FILENAME, ".zip")
        file.copy(from = source_file, to = destination_file, overwrite = TRUE)
      }

      ## if (INPUT_FORMAT == "zip")
      ##   unzip(paste0(DATA_PATH, "primary/", LEGACY_FILENAME, ".zip"),
      ##         overwrite = TRUE, junkpaths = TRUE, exdir = paste0(DATA_PATH, "primary/"))
      if (INPUT_FORMAT == "zip"){
          unzip(paste0(DATA_PATH, "primary/", LEGACY_FILENAME, ".zip"),
                overwrite = TRUE, junkpaths = TRUE, exdir = paste0(DATA_PATH, "primary/"))
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
      if (!DEBUG_MODE) cli::cli_alert_success("Benthic data successfully read into: {.file {paste0(DATA_PATH, 'primary/')}}")

      if (DEBUG_MODE) reefCloudPackage::change_status(stage = paste0("STAGE", CURRENT_STAGE),
                                                      item = "Legacy data",
                                                      ## label = "Legacy data",
                                                      status = "success")
    ## },
    ## logFile = LOG_FILE,
    ## Category = "--Data processing routines--",
    ## msg = "Loading legacy benthic data from supplied bucket",
    ## stage = paste0("STAGE", CURRENT_STAGE),
    ## item = "Legacy data"
    ## )
  } else {
    LEGACY_DATA <<- FALSE
    ## reefCloudPackage::remove_predicates()
  }
  ## reefCloudPackage::save_status()

  ## If DOMAIN_CATOGORY == "tier" then we need to also retrieve the Tiers
  if (DOMAIN_CATEGORY == "tier") {
    ## Get tiers data =================================================
    ## reefCloudPackage::ReefCloud_tryCatch({
      TIERS <<- NULL
      ## Extract spatial data from geoserver
      # if (!(DATA_FROM %in% c("SYNTHETIC","User defined"))) {
      if (TRUE) {
        for (t in 2:5) {
          ## cli_h3(paste0("Extracting geojson data for tier ", t))
          ## system(paste0("curl -w '@curl-format.txt' -o ", DATA_PATH,"/primary/result.json --compressed -X 'GET' 'https://api.dev.reefcloud.ai/reefcloud/dashboard-api/country-features/",DOMAIN_NAME,"?tier_level=",t,"' -H 'accept: application/json'"))

          # system(paste0("curl -o ", DATA_PATH,"primary/result.gz -X 'GET' -sH 'Accept-encoding: gzip' 'https://api.dev.reefcloud.ai/reefcloud/dashboard-api/country-features/",DOMAIN_NAME,"?tier_level=",t,"'"))
          # cli_h3(paste0("Unzipping geojson data for tier ", t))
          # system(paste0("gunzip -kc ", DATA_PATH, "primary/result.gz > ", DATA_PATH, "primary/result.json"))
          if (!DEBUG_MODE) cli_h3(paste0("Importing geojson data for tier ", t))
          tier.sf <- geojson_sf(paste0(AWS_PATH, "raw/tier-",t,".json")) %>%
            suppressMessages() %>%
            suppressWarnings()

          if (nrow(tier.sf)==0) {
            next
          } else {
            if (t!=5) tier.sf <- tier.sf %>% dplyr::select(-reef_area)
            TIERS <<- c(TIERS, paste0('tier',t))
            tier.sf <- tier.sf %>%
              ## filter(tier == t) %>%
              dplyr::mutate( !!(paste0("Tier",t)) := factor(tier_id))
            save(tier.sf, file = paste0(DATA_PATH, "/primary/tier", t, ".sf.RData"))
            if (!DEBUG_MODE) cli_h3(paste0("Make a figure for tier ", t))
            ## reefCloudPackage::make_tiers_figures(tier.sf)
          }
        }
      }
      if (DATA_FROM == "S3") {
        for (i in 2:5) {
          reefCloudPackage::load_aws(file = "tiers", i, ".zip", level = "primary/GIS")
          unzip(paste0(DATA_PATH, "primary/GIS/tiers", i, ".zip"), list = FALSE,
                exdir = paste0(DATA_PATH, "primary/GIS"))
        }
      }

      if (GENERATE_REPORT) {
        ANALYSIS_STAGE <<- c(ANALYSIS_STAGE,
                             list(list(type='component', value = '31b_load_tiers'))) %>%
          unique()
        save(ANALYSIS_STAGE, file=paste0(DATA_PATH, "analysis_stage.RData"))
      }
      ## Need to make sure that TIERS are added to the settings
      ## reefCloudPackage::add_status(1, item="TIERS", label="TIERS",
      ##                              status="SUCCESS", update_display = FALSE)
      ## reefCloudPackage::add_status(CURRENT_STAGE, item="TIERS", label="TIERS",
      ##                              status="SUCCESS", update_display = FALSE)
    ## },
    ## logFile = LOG_FILE,
    ## Category = "--Data processing routines--",
    ## msg = "Loading tier data from supplied bucket",
    ## stage = paste0("STAGE", CURRENT_STAGE),
    ## item = "Retrieve tier data"
    ## )


    ## Try to load covariates from flat files  ========================
    ## reefCloudPackage::ReefCloud_tryCatch({
      files <- list.files(path = paste0(AWS_PATH, "raw"), pattern = "*.csv$", full.names = TRUE)
      files <- gsub("//", "/", files)
      if (length(files)>0) {
        HAS_COVARIATES <<- TRUE
        for (f in files) {
          covariate <- read_csv(f)
          save(covariate, file = paste0(DATA_PATH, "primary/covariate_",
                                        sub(".csv", "", basename(f)), ".RData"))
        }
        if (!DEBUG_MODE)
          cli::cli_alert_success("Covariates data successfully read into: {.file {paste0(DATA_PATH, 'primary/')}}")
        if (GENERATE_REPORT) {
          ANALYSIS_STAGE <<- c(ANALYSIS_STAGE,
                               list(list(type='component', value = '31c_load_covariates'))) %>%
            unique()
          save(ANALYSIS_STAGE, file=paste0(DATA_PATH, "primary/analysis_stage.RData"))
        }
      }
      ## reefCloudPackage::add_status(1, item="HAS_COVARIATES", label="Has covariates",
      ##                              status="SUCCESS", update_display = FALSE)
  ##   },
  ##   logFile = LOG_FILE,
  ##   Category = "--Data processing routines--",
  ##   msg = "Loading covariate data from supplied bucket",
  ##   stage = paste0("STAGE", CURRENT_STAGE),
  ##   item = "Save covariate data")
  }

  ## Unzip and load the Coral Reefs of the World code
  if (DOMAIN_CATEGORY == "tier") {
    ## reefCloudPackage::ReefCloud_tryCatch({
      ## Retrieve a more local version of the data
      if (!DEBUG_MODE) cli_h1("Loading coral reefs of the world shapefile")
      unzip("../parameters/TropicalCoralReefsOfTheWorld.zip", list = FALSE, overwrite = TRUE,
            exdir = paste0(DATA_PATH, "primary/"))
      reef_layer.sf <- read_sf(paste0(DATA_PATH, "/primary/reef_500_poly.shp"))
      save(reef_layer.sf, file = paste0(DATA_PATH, "/primary/reef_layer.sf.RData"))
      sf_files <- list.files(path = paste0(DATA_PATH, "/primary/"),
                             pattern = "reef_500_poly.*",
                             full.names = TRUE)
      invisible(file.remove(sf_files))
  ##   },
  ##   logFile = LOG_FILE,
  ##   Category = "--Data processing routines--",
  ##   msg = "Retrieve reef layer from /parameters",
  ##   stage = paste0("STAGE", CURRENT_STAGE),
  ##   item = "Retrieve reef layers")
  }


  ## reefCloudPackage::save_status()
}
