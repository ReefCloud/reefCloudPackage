#' @title Get violating rows from validation result
#' @description Identify and return rows in the data that violate specified validation rules.
#' @param result The result object returned by `validate::confront()`.
#' @return A data frame of rows violating rules, or NULL if no violations are found.
#' @examples
#' rules <- validator(!is.na(SITE_ID), COVER >= 0)
#' result <- confront(benthic_data, rules)
#' violating_rows(result)
#' @author Murray Logan
#' @export
violating_rows <- function(result) {
  s <- validate::summary(result)
  wch <- which(s["items"] > 1 & s["fails"] > 1) 
  if (length(wch) == 0) {
    return(NULL)
  } else {
    return(validate::violating(data, result[wch]))
  }
}
