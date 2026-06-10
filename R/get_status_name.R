#' @title Get Status Name from Status Tracking
#' @description
#' Retrieves the status name associated with a given stage and item from the status tracking object.
#' @param stage Integer. The stage number within the status tracking object.
#' @param item Character. The name of the item within the specified stage.
#' @return A character string representing the status name associated with the given item and stage.
#' @examples
#' \dontrun{
#' get_status_name(stage = 3, item = "load_benthic_data")
#' }
#' @author Murray Logan
##' @export
 get_status_name <- function(stage, item){
    status_$status[[stage]]$names[which(status_$status[[stage]]$item == item)]   
 }
