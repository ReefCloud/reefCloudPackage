#' @title Modify the STATUS global variable
#' @description This function modifies the STATUS global variable
#' @param stage current running stage
#' @param item name of item to be changed
#' @param status new value assigned to STATUS[[stage]]$item
#' @param update_display Bool: if TRUE prompts update in console. Default TRUE
#' @return void. Modifies STATUS only
#' @examples change_status(stage = "SETTINGS", item = "DEBUG_MODE",
#' status = "success", update_display = FALSE)
##' @export
change_status <- function(stage, item, status, update_display = TRUE) {
    STATUS[[stage]]$status[which(STATUS[[stage]]$item == item)] <- status
    assign("STATUS", STATUS, env = globalenv())
    if (exists("DATA_PATH")) reefCloudPackage::save_status()
    ## if (update_display) reefCloudPackage::openingBanner()  # Disabled - uses legacy STATUS object
}
