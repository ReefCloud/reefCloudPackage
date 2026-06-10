#' @title Validate data against rules
#' @description Validate a data frame using predefined rules, save failing rows if any, and return a tidy summary.
#' @param data A data frame to be validated.
#' @param rules A set of validation rules created with `validator()`.
#' @return A tidy data frame summarising rule descriptions and validation results. Stops with an error if validation fails.
#' @examples
#' rules <- validator(!is.na(SITE_ID), COVER >= 0)
#' validate_data(benthic_data, rules)
#' @author Murray Logan
#' @export
validate_data <- function(data, rules) {
  result <- validate::confront(data, rules)
  invalid_rows <- reefCloudPackage::violating_rows(result)
  val <-   validate::summary(result) |>
    tibble::as_tibble()|>
    dplyr::mutate(description = validate::description(rules)) |>
    dplyr::select(description, everything(), -name, -expression) |>
    data.frame()
  if (any(val$fails)) {
    saveRDS(invalid_rows,
      file = paste0(AWS_OUTPUT_PATH, "invalid_rows.rds")
    )
    stop(paste0("Validation failed: ",
      val$description[val$fails],
      collapse = "\n"
    ))
  } else {
    return(val)
  }
}
