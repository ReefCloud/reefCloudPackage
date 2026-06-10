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
validate_rules <- function() {

## Create a list of rules to check if the required columns are present
benthic_rules <- tibble::tribble(
  ~name,    ~label, ~description, ~rule,
  "P_CODE", "P_CODE missing", "P_CODE column is missing", '"P_CODE" %in% names(.)',
  "P_CODE char", "P_CODE not character", "P_CODE column is not character", 'is.character(P_CODE)',

  "ID", "ID missing", "ID column is missing", '"ID" %in% names(.)',
  "ID dbl", "ID not numeric", "ID column is not numeric", 'is.numeric(ID)',

  "SITE_NAME", "SITE_NAME missing",  "SITE_NAME column is missing", '"SITE_NAME" %in% names(.)',
  "SITE_NAME char", "SITE_NAME not character", "SITE_NAME column is not character", 'is.character(SITE_NAME)',

  "LATITUDE", "LATITUDE missing",  "LATITUDE column is missing", '"LATITUDE" %in% names(.)',
  "LATITUDE dbl", "LATITUDE not numeric", "LATITUDE column is not numeric", 'is.numeric(LATITUDE)',
  "LATITUDE invalid range", "LATITUDE invalid range", "LATITUDE column invalid range", 'in_range(LATITUDE, min=-90, max=90)',

  "LONGITUDE", "LONGITUDE missing",  "LONGITUDE column is missing", '"LONGITUDE" %in% names(.)',
  "LONGITUDE dbl", "LONGITUDE not numeric", "LONGITUDE column is not numeric", 'is.numeric(LONGITUDE)',
  "LONGITUDE invalid range", "LONGITUDE invalid range", "LONGITUDE column invalid range", 'in_range(LONGITUDE, min=-180, max=180)',

  "SITE_NO", "SITE_NO missing", "SITE_NO column is missing", '"SITE_NO" %in% names(.)',
  "SITE_NO char/num", "SITE_NO not character/numeric", "SITE_NO column is not character/numeric", 'is.character(SITE_NO) | is.numeric(SITE_NO)',

  "TRANSECT_NO", "TRANSECT_NO missing", "TRANSECT_NO column is missing", '"TRANSECT_NO" %in% names(.)',
  "TRANSECT_NO char/num", "TRANSECT_NO not character/numeric", "TRANSECT_NO column is not character/numeric", 'is.character(TRANSECT_NO) | is.numeric(TRANSECT_NO)',

  "SITE_DEPTH", "SITE_DEPTH missing", "SITE_DEPTH column is missing", '"SITE_DEPTH" %in% names(.)',
  "SITE_DEPTH char/num", "SITE_DEPTH not character/numeric", "SITE_DEPTH column is not character/numeric", 'is.character(SITE_DEPTH) | is.numeric(SITE_DEPTH)',

  "REEF_ZONE", "REEF_ZONE missing", "REEF_ZONE column is missing", '"REEF_ZONE" %in% names(.)',
  "REEF_ZONE char", "REEF_ZONE not character", "REEF_ZONE column is not character", 'is.character(REEF_ZONE)',

  "REPORT_YEAR", "REPORT_YEAR missing", "REPORT_YEAR column is missing", '"REPORT_YEAR" %in% names(.)',
  "REPORT_YEAR", "Report year > 1000", "Report year is less than 1000", 'REPORT_YEAR > 1000',

  "SURVEY_DATE", "SURVEY_DATE missing", "SURVEY_DATE column is missing", '"SURVEY_DATE" %in% names(.)',
  "SURVEY_DATE invalid", "SURVEY_DATE invalid", "SURVEY_DATE column is invalid", 'is.Date(SURVEY_DATE) | is.POSIXct(SURVEY_DATE)',

  "FRAME", "FRAME missing",  "FRAME column is missing", '"FRAME" %in% names(.)',
  "FRAME char", "FRAME not character", "FRAME column is not character", 'is.character(FRAME)',

  "POINT_NO", "POINT_NO missing", "POINT_NO column is missing", '"POINT_NO" %in% names(.)',
  "POINT_NO dbl", "POINT_NO not numeric", "POINT_NO column is not numeric", 'is.numeric(POINT_NO)',
  "POINT_NO invalid", "POINT_NO not valid", "POINT_NO column is not valid", 'POINT_NO > 0',

  "GROUP_DESC", "GROUP_DESC missing",  "GROUP_DESC column is missing", '"GROUP_DESC" %in% names(.)',
  "GROUP_DESC char", "GROUP_DESC not character", "GROUP_DESC column is not character", 'is.character(GROUP_DESC)',

  "BENTHIC_CATEGORY", "BENTHIC_CATEGORY missing",  "BENTHIC_CATEGORY column is missing", '"BENTHIC_CATEGORY" %in% names(.)',
  "BENTHIC_CATEGORY char", "BENTHIC_CATEGORY not character", "BENTHIC_CATEGORY column is not character", 'is.character(BENTHIC_CATEGORY)',
  )
benthic_rules <- validate::validator(.data = benthic_rules)
return(benthic_rules)
assign("benthic_rules", benthic_rules, envir = .GlobalEnv)
}


