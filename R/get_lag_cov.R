#' @title Get lagged disturbance value
#' @description Filter disturbance values to extract lag from a previous year
#' @param yr Adjusted year of the ecological survey
#' @param cov Extracted disturbance layer corresponding to the monitored years
#' @param tier5 Spatial unit identifier
#' @param .x Column name from the disturbance data (e.g. severity or max variable)
#' @return Numeric value of the lagged disturbance, or 0 if missing
#' @examples
#' get_lag_cov(yr = 2020, cov = cov_dhw, tier5 = 101, .x = "severity_dhw")
#' @author Julie Vercelloni
#' @export

get_lag_cov <- function(yr, cov, tier5, .x) {
  val <- cov %>% filter(Tier5 == tier5, year == yr) %>% pull(.x)
  return(ifelse(length(val) == 0, 0, val))
}