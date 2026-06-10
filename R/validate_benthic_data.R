#' @title Validate benthic data
#' @description Validate the input benthic data against a set of predefined rules
#' @param data Benthic data
#' @param rules Rules to validate the data
#' @return Validation result or the violating rows of the benthic data
#' @examples
#' rules <- validator(!is.na(SITE_ID), COVER >= 0)
#' validate_benthic_data(benthic_data, rules)
#' @author Murray Logan
#' @export
validate_benthic_data <- function(data, rules) {
  status::status_try_catch(
  {
    reefCloudPackage::validate_data(data, rules)
  },
  stage_ = 2,
  name_ = "Validate benthic data",
  item_ = "validate_benthic_data"
  )
}
