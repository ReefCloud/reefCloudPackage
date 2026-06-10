#' @title Add covariates to data
#'
#' @description 
#' This function merges a covariate layer (typically representing disturbance) with monitoring data. 
#' It adjusts covariate values based on the timing of the surveys, computes lagged versions of the 
#' covariate (e.g., 1-year and 2-year lags), and handles missing values by replacing them with 0.
#'
#' The adjustment logic ensures that if the disturbance occurred after the survey date, 
#' the covariate value from the previous year is used instead.
#'
#' @param data A data frame containing the monitoring data (including fields like `Tier5`, `REPORT_YEAR`, `DATE`, etc.)
#' @param cov A data frame containing the extracted disturbance layer values by year and `Tier5` spatial unit
#' @param cov_name A string naming the disturbance layer, used to construct column names such as `severity_[name]`
#'
#' @return A data frame with added and adjusted covariate columns: 
#' `severity_[name]`, `max_[name]`, and their lagged versions for 1 and 2 years.
#'
#' @examples 
#' # Example usage:
#' # data <- readRDS("monitoring_data.rds")
#' # cov <- readRDS("disturbance_layer.rds")
#' # data_with_cov <- add_cov_to_data(data, cov, cov_name = "cyclone")
#' @author Julie Vercelloni
#' @keywords internal
#' @noRd

add_cov_to_data_old <- function(data, cov, cov_name) {
  data %>%  
    dplyr::left_join(cov, by = c("Tier5" = "Tier5",
                          "REPORT_YEAR" = "year")) %>%
    dplyr::mutate(across(paste0(c("severity_", "max_"), cov_name),
                  ~ replace_na(.x, replace = 0))) %>% 
    dplyr::group_by(REEF, SITE_NO, TRANSECT_NO, fDEPTH, fGROUP) %>% 
    dplyr::arrange(fYEAR) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(across(paste0(c("severity_", "max_"), cov_name),
                  list(~adjust_cov_for_after_surveys(DATE, end_date, cov, Tier5, cur_column())),
                  .names = "{.col}")) %>% 
    dplyr::mutate(across(paste0(c("severity_", "max_"), cov_name),
                  list(lag1 =  ~ get_lag_cov(REPORT_YEAR-1, cov, Tier5, cur_column())),
                  .names = "{.col}.{.fn}")) %>% 
    dplyr::mutate(across(paste0(c("severity_", "max_"), cov_name),
                  list(lag2 =  ~ get_lag_cov(REPORT_YEAR-2, cov, Tier5, cur_column())),
                  .names = "{.col}.{.fn}")) %>% 
    dplyr::select(-end_date) %>% 
    dplyr::ungroup()
}
