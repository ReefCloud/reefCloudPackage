#' @title Initialise log
#' @description The following function initialises a log file. This log file is placed in the root of the project as it needs to be in a location that is guarenteed to exist from a freshly cloned instance of this codebase.
#' @return creates log file\cr
#' and the globabal variable LOG_FILE, which contains file location
#' @examples initialise_log
#' @export
initialise_log <- function() {
  status::status_try_catch(
  {
    ##Log file
    LOG_FILE <<- paste0(DATA_PATH, 'log/', FILENAME, '.log')
    ## LOG_FILE <<- paste0("../.mmp.reefCloudPackage::log")
    if (file.exists(LOG_FILE)) unlink(LOG_FILE)
    ## reefCloudPackage::add_status(stage = "SETTINGS", item = "LOG_FILE",
    ##                       label = "Log file", status = "success",
    ##                       update_display = FALSE)
  # Always add/update log_file setting (status package handles existing settings)
  tryCatch({
    status::update_setting("log_file", LOG_FILE)
  }, error = function(e) {
    status::add_setting("log_file", LOG_FILE, "LOG file")
  })
  },
  stage_ = 1,
  name_ = "Initialise log",
  item_ = "initialise_log"
  )
}

