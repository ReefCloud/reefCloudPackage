

#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
whratio <- function(object.sf) {
    bb <- object.sf %>% st_bbox()
    w <- bb[3] - bb[1]
    h <- bb[4] - bb[2]
    w/h
}
