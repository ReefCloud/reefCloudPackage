#' @title Is Parent?
#' @description The following function determines whether the current script is the ##
#' parent (directly called from command line etc) or child (sourced from another R script.
#' @return TRUE if is parent, FALSE if not
#' @examples isParent()
#' @export
isParent <- function() {
  ifelse(sys.nframe()==1, TRUE, FALSE)
}
