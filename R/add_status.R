#' @title add_status
#' @description modify STATUS global variable
#' @param stage integer indicating current stage
#' @param item item to be added
#' @param label name of item to be added
#' @param update_display boolean indicating if change to STATUS displayed on screen
##' @export
add_status <- function(stage, item, label, status, update_display = TRUE) {
    STATUS[[stage]]$items <- c(STATUS[[stage]]$items, item)
    STATUS[[stage]]$labels <- c(STATUS[[stage]]$labels, label)
    STATUS[[stage]]$status <- c(STATUS[[stage]]$status, status)
    STATUS[[stage]]$predicate <- c(STATUS[[stage]]$predicate, NA)
    STATUS[[stage]]$predicate_value <- c(STATUS[[stage]]$predicate_value, NA)
    assign("STATUS", STATUS, env = globalenv())
    if (exists("DATA_PATH")) reefCloudPackage::save_status()
    ## if (update_display) reefCloudPackage::openingBanner()  # Disabled - uses legacy STATUS object
}
