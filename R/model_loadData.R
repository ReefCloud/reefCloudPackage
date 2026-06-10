#' @title Load data
#' @description This script will copy all the data sources from the bucket provided and place in <DATA_PATH>/primary.\cr
#' When run on the dev server (HPC) DATA_FROM will be LOCAL\cr
#' When run on the prod server (AWS) DATA_FROM will be S3\cr
#' There should be three data collections in <AWS_PATH>/raw:\cr
#' - benthic data (reefs_data.csv|zip)\cr
#' - spatial tiers data (tiers[2-5].shp)\cr
#' - covariates data (?)
#' @examples model_loadData()
#' @author Murray Logan
#' @export
model_loadData <- function(){

  if(.Platform$OS.type == "unix") {
    reefCloudPackage::model_loadData_unix()
  } else if (.Platform$OS.type == "windows"){
    msg <- paste("Pipeline is not supported on Windows. Execution stopped.")
   ## ##  status:::status_log (  # Disabled - private function not exported"ERROR", log_file = log_file, "--Model load data--", msg = msg)  # Disabled - private function
    stop(msg)
    #reefCloudPackage::model_loadData_windows()
  }
}
