#' @title add_stage
#' @description Adds stage to STATUS global variable
#' @param stage stage id (integer)
#' @param title name of stage (character)
#' @return The global variable STATUS is modified
#' @examples examples
#' @export
add_stage <- function(stage, title) {
    STATUS[[stage]]$title <- title
    assign("STATUS", STATUS, env = globalenv())
    if (exists("DATA_PATH")) reefCloudPackage::save_status()
}
