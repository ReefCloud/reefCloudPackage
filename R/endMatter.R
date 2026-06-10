#' @title endMatter
#' @description Closing banner
#' @export
endMatter <- function() {
    cat("\n***************************************************\n")
    cat("Log:\n")
    logs <- readLines(paste0(DATA_PATH, "/log/reef_data.log"))
    logs <- paste0(paste0(logs, collapse = "\n"), "\n")
    cat(logs)
    cat("***************************************************\n\n")
    cat("THE END!\n")
}
