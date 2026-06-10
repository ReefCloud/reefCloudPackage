#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
##' @export
read_status <- function() {
    STATUS <- readRDS(file = paste0(DATA_PATH, "STATUS.RData"))
    assign("STATUS", STATUS, env = globalenv())
    values <- readRDS(file = paste0(DATA_PATH, "global_settings.RData"))
    list2env(values, envir = globalenv())
}
