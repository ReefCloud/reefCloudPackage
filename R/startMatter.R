#' @title startMatter
#' @description The following function is a wrapper around a number of routines \cr
#' that occur once at the start of an analysis run. In order, these routines are:\cr
#' - initialise a status list.  This list is the basis of information presented in the command line interface.\cr
#' - initialise the log file\cr
#' @param args command line arguments. The call must be of the form:\cr
#' "--bucket="<PATH> --domain=<DOMAIN> --by_tier=<NUMBER> --debug=<true|false> --runStage=<NUM> --refresh_data=<true|false>"\cr
#' <PATH>:  	a valid path to a folder containing the input data\cr
#' <DOMAIN>:	either tier (spatio-temporal model for the data provide) or site (hierarchical model for a single site.\cr
#' <NUMBER>:	for tier analyses, an optional tier number to indicate which tier level to use in splitting up analyses \cr
#' <true|false>:	 whether to operate in debug mode. If true, progress is provided via a CLI\cr
#' <NUM>:	 which stages of the analysis to run (-1 or missing is all stages)
#' @return Creates the following global variables (created by parseCLA)\cr
#' REFRESH_DATA\cr
#' DEBUG_MODE\cr
#' runstage\cr
#' runStage\cr
#' AWS_PATH\cr
#' DATA_FROM\cr
#' DOMAIN_CATEGORY \cr
#' AWS_OUTPUT_PATH \cr
#' DOMAIN_NAME \cr
#' GENERATE_REPORT \cr
#' MODEL_TYPE \cr
#' BY_TIER\cr
#' \cr
#' and (created by startMatter): \cr
#' CURRENT_STAGE\cr
#'
#' @examples startMatter(args = c("--bucket=/home/data/AUS", "--domain=tier", "--by_tier=4", "--debug=true", "--runStage=1", "--refresh_data=false"))
#' @export
startMatter <- function(args = commandArgs()) {
  ## reefCloudPackage::initialise_status()    ## create the status list
  status::status_initialize(
    pkg = "reefCloudPackage",
    project_name = "ReefCloud statistical modelling",
    box_width = 100
  ) ## create the status list
  status::status_set_stage(stage = 1, title = "Configure system")
  status::display_status_terminal()        ## display an opening banner
  reefCloudPackage::parseCLA(args)         ## parse command line arguments
  ## Start by clearing all local data folders
  if (REFRESH_DATA) reefCloudPackage::clear_data()
  CURRENT_STAGE <<- 1
  reefCloudPackage::generateSettings()     ## generate the rest of the path and naming settings 
  reefCloudPackage::initialise_log()       ## create the log file
  reefCloudPackage::config()               ## create directory structure if it does not yet exist
  reefCloudPackage::checkPackages()        ## load required packages
  reefCloudPackage::analysis_stage()       ## read in the stage that the analysis is up to
  ## reefCloudPackage::openingBanner()        ## display an opening banner
  status::display_status_terminal()        ## display an opening banner
  ## reefCloudPackage::save_status()

  ## function to go in the status package 
  get_status_name <- function(stage, item){
    status_$status[[stage]]$names[which(status_$status[[stage]]$item == item)]   
 } 
}
