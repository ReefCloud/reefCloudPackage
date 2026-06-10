#' @title loadRData
#' @description Reads RData, and assigns a name
#' @param fileName .RData file name
#' @examples loadRData(".RData")
#' @export
loadRData <- function(fileName){
    ##loads an RData file, and returns it
    load(fileName)
    get(ls()[ls() != "fileName"])
}
