#' @title config
#' @description creates directory structure if it does not yet exist
#' @return Creates the following global variables \cr
#' @examples config()
#' @export
config <- function(){
  ## If DOMAIN_NAME is 'synthetic', we may need to create the path
  if (!dir.exists(AWS_PATH)) dir.create(AWS_PATH)

  ## Create the general data path if it does not exists
  if (!dir.exists(DATA_PATH)) dir.create(DATA_PATH)

  ## Create the reefCloudPackage::log file (if it does not exits)
  if (!dir.exists(paste0(DATA_PATH, 'log'))) dir.create(paste0(DATA_PATH, 'log'))
  if (file.exists(LOG_FILE)) file.remove(LOG_FILE)
  if (!file.exists(LOG_FILE)) file.create(LOG_FILE)
  if (!dir.exists(paste0(AWS_PATH, "outputs"))) dir.create(paste0(AWS_PATH, "outputs"))
  if (!dir.exists(AWS_OUTPUT_PATH)) dir.create(AWS_OUTPUT_PATH)
  unlink(paste0(AWS_OUTPUT_PATH, "*.*"), recursive = TRUE)

  ## Create the directory structure
  status::status_try_catch(
  {
    if (!dir.exists(paste0(DATA_PATH, '/primary'))) dir.create(paste0(DATA_PATH, '/primary'))
    if (!dir.exists(paste0(DATA_PATH, '/processed'))) dir.create(paste0(DATA_PATH, '/processed'))
    if (!dir.exists(paste0(DATA_PATH, '/modelled'))) dir.create(paste0(DATA_PATH, '/modelled'))
    if (!dir.exists(paste0(DATA_PATH, '/summarised'))) dir.create(paste0(DATA_PATH, '/summarised'))

    OUTPUT_PATH <<- "../output/"
    if (!dir.exists(OUTPUT_PATH)) dir.create(OUTPUT_PATH)
    if (!dir.exists(paste0(OUTPUT_PATH, 'figures'))) dir.create(paste0(OUTPUT_PATH, 'figures'))
    if (!dir.exists(paste0(OUTPUT_PATH, 'figures/spatial/'))) dir.create(paste0(OUTPUT_PATH, 'figures/spatial/'))

    DOCS_PATH <<- "../docs/"
    ## Dockerfile will handle adding the necessary files

    ## reefCloudPackage::change_status(stage = "STAGE1", item = "Prepare file system", status = "success",
    ##                          update_display = FALSE)
    #status::add_status_item(stage = 1, item = "prepare_file_system", name = "Prepare file system", status = "success")
  },
  stage_ = 1,
  name_ = "Configure paths",
  item_ = "configure_paths"
  )

}

