#' @title Load Data from S3 or Local Sources
#' @description Copies all required data sources from the bucket or local path into `<DATA_PATH>/primary`.\cr
#' - On the dev server (HPC), `DATA_FROM` must be `"LOCAL"`.\cr
#' - On the prod server (AWS), `DATA_FROM` must be `"S3"`.\cr
#' Required data collections under `<AWS_PATH>/raw` include:\cr
#' • Benthic data (`reefs_data.csv` or `.zip`)\cr
#' • Spatial tiers data (`tiers[2-5].shp`)\cr
#' • Covariates data (format TBD)
#' @param None This function uses global settings and paths.
#' @return NULL All datasets are saved to disk; nothing is returned.
#' @examples
#' model_loadData_unix()
#' @author Murray Logan
#' @export
model_loadData_unix <- function(){
  if (reefCloudPackage::isParent()) reefCloudPackage::startMatter()
  status::status_set_stage(stage = 2, title = "Obtain data")

  ## Benthic data =============================================================
  ## 1. Retrieve the benthic data from the S3 bucket
  reefCloudPackage::retrieve_benthic_data()

  ## 2. Unzip data
  reefCloudPackage::unzip_benthic_data()
  
  ## 3. Import data
  data <-  reefCloudPackage::import_benthic_data() 

  ## 4. Validate data
  rules <- reefCloudPackage::validate_rules()
  result <- reefCloudPackage::validate_benthic_data(data, rules) 
  rm(rules, result)

  ## 4. Save data
  reefCloudPackage::save_benthic_data(data)
  
  ## Retrieve legacy benthic data (if it exists) ===================================================
  ## The legacy data may have an additional field ('cover') that records
  ## the percentage cover of the benthos.  If this field is non-NA,
  ## then the 'frame' and 'point_no' fields will be NA
  LEGACY_FILENAME <<- 'legacy_data'
  LEGACY_DATA <<- FALSE
  if(file.exists(paste0(AWS_PATH, 'raw/', LEGACY_FILENAME, '.zip'))) {
    reefCloudPackage::get_legacy_data()
  } else {
    LEGACY_DATA <<- FALSE
    ## status::remove_status_item(stage = 2, item = "legacy_data")  # Function removed in status package
  }

  ## If DOMAIN_CATOGORY == "tier" then we need to also retrieve the Tiers
  if (DOMAIN_CATEGORY == "tier") {
    ## Get tiers data =================================================
    reefCloudPackage::get_tiers()
  }

  ## Try to load covariates from geoserver  ========================

  reefCloudPackage::get_covariates()
  
  ## Unzip and load the Coral Reefs of the World code
  if (DOMAIN_CATEGORY == "tier") {
    null <- reefCloudPackage::get_coral_reef_shape_files()
  }
}
