
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
compare_predicates <- function(PREDICATE, VALUE) {
    if (class(PREDICATE) == 'logical') {
        return(PREDICATE != as.logical(VALUE))
    } else {
        return(PREDICATE != VALUE)
    }
}
