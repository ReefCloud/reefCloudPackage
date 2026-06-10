#' @title Save benthic data
#' @description Save benthic data to an RData file in the primary data directory. Optionally updates the analysis stage.
#' @param data A data frame or list to be saved
#' @return NULL (used for side effects: writing files and updating state)
#' @examples
#' save_benthic_data(benthic_df)
#' @author Murray
#' @export

save_benthic_data <- function(data) {
  status::status_try_catch(
  {
    ## Save as native R file
    save(data, file = paste0(DATA_PATH, "primary/", RDATA_FILE))
    if (!DEBUG_MODE) cli_alert_success("Benthic data successfully read into: {.file {paste0(DATA_PATH, 'primary/')}}")
    if (GENERATE_REPORT) {
      ANALYSIS_STAGE <<- c(ANALYSIS_STAGE,
        list(list(type='component', value = '31a_load_benthos'))) %>% 
        unique()
      save(ANALYSIS_STAGE, file=paste0(DATA_PATH, "analysis_stage.RData"))
    }
  file.remove(paste0(DATA_PATH, "primary/", CSV_FILE))
  },
  stage_ = 2,
  name_ = "Save benthic data",
  item_ = "save_data"
  )
}
