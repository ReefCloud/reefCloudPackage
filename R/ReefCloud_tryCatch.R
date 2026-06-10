#' @title tryCatch function for the reefCloudPackage
#' @description des
#' @param expr expression to be evaluated.
#' @param logFile path for log file
#' @param Category ask
#' @param expectedClass ask
#' @param msg message
#' @param return ask
#' @param showWarnings Bool. Show warnings (TRUE) or not (FALSE).
#' @param stage ask
#' @param item ask
#' @return updates Log file according to
#' @export

ReefCloud_tryCatch <- function(expr, logFile,Category, expectedClass=NULL, msg=NULL, return=NULL,
                     showWarnings=FALSE, stage = NULL, item = NULL) {
  if (DEBUG_MODE & !is.null(stage)) reefCloudPackage::change_status(stage = stage, item = item, status = "progress")
  if (!exists('PROGRESS')) PROGRESS=NULL
  max.warnings<-4
  warnings<-0
  W <- NULL
  w.handler <- function(w){ # warning handler
    m<-w$message
    print(m)
    if ((warnings < max.warnings) && (grepl ('WARNING', m)>0)) {
      reefCloudPackage::log('WARNING', logFile, Category, paste(warnings, msg, m))
      warnings<<-warnings+1
    }
    invokeRestart("muffleWarning")
  }
  ## ret <- list(value = withCallingHandlers(tryCatch(expr,
  ##                                                  error = function(e) e,
  ##                                                  warning = function(w) w,
  ##                                                  message = function(m) m),
  ##                                         warning = w.handler),warning = W)
  ret <- list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
                                          warning = w.handler),warning = W)
  ## if(!is.atomic(ret$value) && !is.null(ret$value$message)){
  ## print(!is.atomic(ret$value))
  ## print(any(class(ret$value) %in% c("simpleError", "error", "rlang_error")))
  if(!is.atomic(ret$value) && any(class(ret$value) %in% c("simpleError", "error", "rlang_error"))){
    ## An error occurred
    PROGRESS <<- c(PROGRESS,'Fail')
    class(ret) <- "try-error"
    if (DEBUG_MODE & !is.null(stage)) reefCloudPackage::change_status(stage = stage, item = item, status = "failure")
    reefCloudPackage::log('ERROR', logFile, Category, paste(msg, ret$value$message))
    if(!is.null(return)) {
      FALSE
    }else {
      if (DEBUG_MODE) {
        "An error occured, please refer to the status line above..."
      } else {
        quit(status=-1,save="no")
      }
    }
  } else {    #no error check for warning
    PROGRESS <<- c(PROGRESS,'Pass')
    if (DEBUG_MODE & !is.null(stage)) reefCloudPackage::change_status(stage = stage, item = item, status = "success")
    reefCloudPackage::log('SUCCESS', logFile, Category, msg)
    if(!is.null(return)) {
      TRUE
    }
  }
}
