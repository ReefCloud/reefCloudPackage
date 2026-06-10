#' @title Import benthic data
#' @description Import benthic data from a CSV file located in the primary data directory
#' @return A data frame with the benthic data
#' @examples
#' benthic_df <- import_benthic_data()
#' @author Murray Logan
#' @export

import_benthic_data <- function() {
  status::status_try_catch(
  {
    ## Read data using vroom for better performance (Suggestion 39)
    data <- vroom::vroom(paste0(DATA_PATH, "primary/", CSV_FILE),
      col_types = "cdcddccdcdTcdcc",
      trim_ws = TRUE,
      show_col_types = FALSE) %>%
      mutate(SITE_DEPTH = as.character(SITE_DEPTH))
  },
  stage_ = 2,
  name_ = "Import benthic data",
  item_ = "import_benthic_data"
  )
  return(data)
}
