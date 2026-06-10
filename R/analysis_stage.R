#' @title Analysis stage
#' @description Read in the stage that the analysis is up to
#' @return returned arguments description
#' @examples examples
#' @export
analysis_stage <- function() {
    if (file.exists(paste0(DATA_PATH, "analysis_stage.RData"))) {
        load(paste0(DATA_PATH, "analysis_stage.RData"))
    } else {
        ANALYSIS_STAGE <<- list(list(type='component', value='01_start'))
        save(ANALYSIS_STAGE, file=paste0(DATA_PATH, "analysis_stage.RData"))
    }
}
