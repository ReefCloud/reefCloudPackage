#' @title Tidy validation results
#' @description Summarise and reformat the results of a data validation check.
#' @param result The result object returned by `validate()`.
#' @param rules The validation rules used in the check.
#' @return A tidy data frame with rule descriptions and validation outcomes.
#' @examples
#' rules <- validator(!is.na(SITE_ID), COVER >= 0)
#' result <- confront(benthic_data, rules)
#' tidy_validation(result, rules)
#' @author Murray Logan
#' @export
tidy_validation <- function(result, rules) {
  summary(result) |>
    mutate(description = description(rules)) |>
    dplyr::select(description, everything(), -name, -expression)
}
