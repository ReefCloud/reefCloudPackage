#' @title Trim years of the predictive layers
#' @description Cut years associated with ecological survey
#' @param full_cov_raw covariates 
#' @param data.grp.tier data grouped
#' @return full_cov
##' @export

trim_years_from_predictive_layers <- function(full_cov_raw, data.grp.tier) {
  #status::status_try_catch(
  {
     full_cov <- full_cov_raw |>
        dplyr::mutate(across(matches("^severity.*|^max.*"), ~replace_na(.x, 0)),
         REPORT_YEAR = year) |>
         dplyr::filter(between(REPORT_YEAR, min(data.grp.tier$REPORT_YEAR), max(data.grp.tier$REPORT_YEAR))) |>
         dplyr::rename(fYEAR = year)
  }#,
 # stage_ = 4,
 # name_ = "Trim predictive layers",
 # item_ = "trim_predictive_layers"
 # )
  return(full_cov)
}