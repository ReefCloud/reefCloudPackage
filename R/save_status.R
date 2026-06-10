#' @title Save status
#' @description save current status and current global variables
#' @return save STATUS IN in DATA_PATH/STATUS.RData file\cr
#' and current values of settings in DATA_PATH/global_settings.RData
#' @examples save_status()
##' @export
save_status <- function() {
    # Legacy function - STATUS object no longer used with status package
    if (!exists("STATUS")) return(invisible(NULL))

    saveRDS(STATUS, file = paste0(DATA_PATH, "STATUS.RData"))
    values <- sapply(STATUS[[1]]$items, function(x) if(exists(x)) eval(parse(text = x)),
                     simplify = FALSE)
    saveRDS(values, file = paste0(DATA_PATH, "global_settings.RData"))
}
