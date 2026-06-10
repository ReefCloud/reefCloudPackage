
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
##' @export
remove_status <- function(stage, item) {
    wch <- which(STATUS[[stage]]$item == item)
    STATUS[[stage]]$items <- STATUS[[stage]]$items[-wch]
    STATUS[[stage]]$labels <- STATUS[[stage]]$labels[-wch]
    STATUS[[stage]]$status <- STATUS[[stage]]$status[-wch]
    STATUS[[stage]]$predicate <- STATUS[[stage]]$predicate[-wch]
    STATUS[[stage]]$predicate_value <- STATUS[[stage]]$predicate_value[-wch]
    assign("STATUS", STATUS, env = globalenv())
    if (exists("DATA_PATH")) reefCloudPackage::save_status()
}
