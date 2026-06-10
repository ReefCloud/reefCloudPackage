
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
remove_predicates <- function(update_display = TRUE) {
    for (i in 1:length(STATUS)) {
        for (j in 1:length(STATUS[[i]]$predicate)) {
            if (!is.na(STATUS[[i]]$predicate[j]) &
                exists(STATUS[[i]]$predicate[j])) { 
                if (is.null(get(STATUS[[i]]$predicate[j]))) next
                if (reefCloudPackage::compare_predicates(
                    get(STATUS[[i]]$predicate[j]),
                    STATUS[[i]]$predicate_value[j]))
                    reefCloudPackage::remove_status(i, STATUS[[i]]$items[j])
            }
            
        }
    }
    ## if (DEBUG_MODE & update_display) reefCloudPackage::openingBanner()  # Disabled - uses legacy STATUS object
}
