#' @title parseCLA
#' @description The following function parses the command line arguments \cr
#' 1. Ensure that a bucket has been supplied as --bucket=<bucket>\cr
#' 2. If the bucket is local (/data/.* or /mnt/reefcloud/.*)\cr
#'    - set DATA_FROM <<- "LOCAL"\cr
#'    If the bucket is S3 (s3://dev-aims-gov-au-reefcloud-stats/reefcloud/.*)\cr
#'    - set DATA_FROM <<- "S3"\cr
#' 3. Ensure that a valid domain has been supplied and if so\cr
##    - set DOMAIN_CATEGORY <<- <domain>\cr
#' @param args command line arguments. The call must be of the form:\cr
#' "--bucket="<PATH> --domain=<DOMAIN> --by_tier=<NUMBER> --debug=<true|false> --runStage=<NUM> --refresh_data=<true|false>"\cr
#' <PATH>:  	a valid path to a folder containing the input data\cr
#' <DOMAIN>:	either tier (spatio-temporal model for the data provide) or site (hierarchical model for a single site.\cr
#' <NUMBER>:	for tier analyses, an optional tier number to indicate which tier level to use in splitting up analyses \cr
#' <true|false>:	 whether to operate in debug mode. If true, progress is provided via a CLI\cr
#' <NUM>:	 which stages of the analysis to run (-1 or missing is all stages)
#' @return Creates the following global variables \cr
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
#' BY_TIER
#' @examples
#' args = c("--bucket=/home/data/AUS", "--domain=tier", "--by_tier=4", "--debug=true", "--runStage=1", "--refresh_data=false")
#' parseCLA(args)
#' @export
parseCLA <- function(args) {
  status::status_try_catch(
  {
  valid_cla <- paste0("The call must be of the form:\n",
    "30_model.R --bucket=\"<PATH>\"",
    "\n\t--domain=<DOMAIN>",
    "\n\t[--by_tier=<NUMBER> --debug=<true|false> --runStage=<NUM>",
    " --refresh_data=<true|false>]\n\n",
    "\n<PATH>:  \ta valid path to a folder containing the input data",
    "\n<DOMAIN>:\teither tier (spatio-temporal model for the data provide)",
    "\n\t\tor site (hierarchical model for a single site.",
    "\n<NUMBER>:\tfor tier analyses, an optional tier number to indicate which ",
    "\n\t\ttier level to use in splitting up analyses",
    "[n>true|false>:\t whether to operate in debug mode.  If true,",
    "\n\t\tprogress is provided via a CLI",
    "\n<NUM>:\t which stages of the analysis to run (-1 or missing is all stages")

  bucket <- grep('--bucket=.*', args)
  domain <- grep('--domain=.*', args)
  by_tier <- grep('--by_tier=.*', args)
  model_type <- grep('--model_type=.*', args)

  REFRESH_DATA <<- ifelse(any(grepl('--refresh_data ?= ?(true|t|TRUE|T)', args, perl = TRUE)), TRUE, FALSE)
  ## reefCloudPackage::change_status(stage = "SETTINGS", item = "REFRESH_DATA",
  ##   status = "success", update_display = FALSE)
  status::add_setting(element = "refresh_data", item = REFRESH_DATA, name = "Refresh data")

  DEBUG_MODE <<- ifelse(any(grepl('--debug ?= ?(true|t|TRUE|T)', args, perl = TRUE)), TRUE, FALSE)
  ## reefCloudPackage::change_status(stage = "SETTINGS", item = "DEBUG_MODE",
  ##   status = "success", update_display = FALSE)
  status::add_setting(element = "debug_mode", item = DEBUG_MODE, name = "Debug mode")

  runstage <<- grep('--runStage ?=.*', args, perl = TRUE)
  runStage <<- gsub('--runStage ?= ?', '\\1', args[runstage])
  if (length(runStage) == 0) {
    ## runStage <<- reefCloudPackage::get_stages()
    runStage <<- status::get_setting(element = "run_stages")
  }
  if (length(runStage) == 1) {
    if (runStage == -1) {
      ## runStage <<- reefCloudPackage::get_stages()
      runStage <<- status::get_setting(element = "run_stages")
    }
  }

  ## Ensure that a bucket is supplied
  if (length(domain) == 0)
    stop(paste0('A bucket (location/path of raw data) needs to be provided as a command line arguement, such as: Rscript <script.R> --bucket=<PATH>\n\n',
      valid_cla
    ))
  file <- args[bucket]
  AWS_PATH <<- gsub('--bucket=(.*)','\\1/', file)
  ## reefCloudPackage::change_status(stage = "SETTINGS", item = "AWS_PATH",
  ##   status = "success", update_display = FALSE)
  status::add_setting(element = "aws_path", item = AWS_PATH, name = "AWS path")

  ## Determine whether data are to be sourced locally or from s3 bucket
  DATA_FROM <<- ifelse(
    grepl('data/synthetic', AWS_PATH, perl = TRUE), 'SYNTHETIC',
    ifelse(grepl('(^/mnt.*|^/home/data.*|^/data/.*)', AWS_PATH, perl=TRUE), 'LOCAL',
      ifelse(grepl('^[sS]3.*', AWS_PATH, perl=TRUE), 'S3', 'User defined')))
  ## reefCloudPackage::change_status(stage = "SETTINGS", item = "DATA_FROM",
  ##   status = "success", update_display = FALSE)
  status::add_setting(element = "data_from", item = DATA_FROM, name = "Data from")

  ## Ensure that valid domain is supplied
  if (length(domain)==0)
    stop(paste0('A domain needs to be provided as a command line arguement, such as: Rscript <script.R> --domain=<DOMAIN>\n\n',
      valid_cla
    ))
  if (!grepl('--domain=(site|tier)',args[domain], perl=TRUE))
    stop("The supplied domain in --domain=<domain> must be either 'site' or 'tier'")

  ## Set domain category global variable
  DOMAIN_CATEGORY <<- gsub('--domain=', '', args[domain])
  ## reefCloudPackage::change_status(stage = "SETTINGS", item = "DOMAIN_CATEGORY",
  ##   status = "success", update_display = FALSE)
  status::add_setting(element = "domain_category", item = DOMAIN_CATEGORY, name = "Domain category")

  ## Set the AWS_OUTPUT_PATH
  AWS_OUTPUT_PATH <<- paste0(AWS_PATH, "outputs/", DOMAIN_CATEGORY,"/")
  ## reefCloudPackage::change_status(stage = "SETTINGS", item = "AWS_OUTPUT_PATH",
  ##   status = "success", update_display = FALSE)
  status::add_setting(element = "aws_output_path", item = AWS_OUTPUT_PATH, name = "AWS output path")

  ## Get the domain name
  DOMAIN_NAME <<- basename(AWS_PATH)
  ## reefCloudPackage::change_status(stage = "SETTINGS", item = "DOMAIN_NAME",
  ##   status = "success", update_display = FALSE)
  status::add_setting(element = "domain_name", item = DOMAIN_NAME, name = "Domain name")

  ## Determine whether a report should be generated
  GENERATE_REPORT <<- FALSE
  ## reefCloudPackage::change_status(stage = "SETTINGS", item = "GENERATE_REPORT",
  ##   status = "success", update_display = FALSE)
  status::add_setting(element = "generate_report", item = GENERATE_REPORT, name = "Generate report")

  ## Model type
  ## Tier level on which to break up analyses
  if (length(model_type) == 0) {
    MODEL_TYPE <<- 2   #simple hierachical
  } else {
    MODEL_TYPE <<- as.numeric(gsub('--model_type=(.*)', '\\1', args[model_type]))
  }
  ## reefCloudPackage::change_status(stage = "SETTINGS", item = "MODEL_TYPE",
  ##   status = "success", update_display = FALSE)
  status::add_setting(element = "model_type", item = MODEL_TYPE, name = "Model type")

  ## Tier level on which to break up analyses
  if (length(by_tier) == 0) {
    BY_TIER <<- 2
  } else {
    BY_TIER <<- gsub('--by_tier=(.*)', '\\1', args[by_tier])
  }
  ## reefCloudPackage::change_status(stage = "SETTINGS", item = "BY_TIER",
  ##   status = "success", update_display = FALSE)
  status::add_setting(element = "by_tier", item = BY_TIER, name = "By tier level")

  if(!exists("LEGACY_DATA")) LEGACY_DATA <<- NULL

  ## reefCloudPackage::remove_predicates(update_display = FALSE)
  ## if (DEBUG_MODE) reefCloudPackage::openingBanner()
  },
  stage_ = 1,
  order_ = 1,
  name_ = "Parse CLA",
  item_ = "parse_cla"
  )

}
