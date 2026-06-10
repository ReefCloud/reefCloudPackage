#' Adjust disturbance values
#' 
#' Adjusting disturbance values depending on survey dates
#' @title adjust_cov_for_after_surveys 
#' @param dt is the date of ecological survey
#' @param cov refers to the extracted disturbance layer corresponding to the monitored years
#' @param end_date is the date associated with max disturbance values
#' @param tier5 is the spatial unit
#' @param .x is the col from the disturbance data 
#' @author Julie Vercelloni
#' @export

adjust_cov_for_after_surveys <- function(dt, end_date, cov, tier5, .x) {
  if (is.na(end_date)) return(0)
  yr <- year(dt)
  yr_1 <- yr - 1
  if (end_date > dt) yr <- yr_1
  return(get_lag_cov(yr, cov, tier5, .x))
}