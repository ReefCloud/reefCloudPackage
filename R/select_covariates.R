#' Select Spatially Representative Covariates
#'
#' Selects covariates from a spatial dataset if their 75th mid-quantile value is greater than 0,
#' assuming such variables are spatially representative.
#'
#' @param x A `sf` dataframe (e.g., `HexPred_sf`) containing covariates.
#' @return A character vector of selected covariate names.
#' @author Julie Vercelloni
#' @examples
#' \dontrun{
#' library(sf)
#' library(dplyr)
#' 
#' # Example mock data
#' hex_grid <- st_sf(
#'   geometry = st_sfc(st_point(c(1, 2)), st_point(c(3, 4))),
#'   max_dhw = c(0.1, 0.5),
#'   max_cyc = c(0, 0),
#'   max_wave = c(1.2, 1.5)
#' )
#' 
#' selected_vars <- select_covariates(hex_grid)
#' print(selected_vars)
#' }
#' @export
select_covariates <- function(x, i , N) {
  filtered_data <- tryCatch(
   {
  variables_name_full <- names(x)
  variables_name_full <- grep("^max", variables_name_full, value = TRUE)

  result <- x |>
    dplyr::select(all_of(variables_name_full)) |>
    st_drop_geometry() |>
    dplyr::summarise(across(everything(), ~ reefCloudPackage::mid_quant_75(.x), .names = "{.col}")) |>
    tidyr::pivot_longer(everything(), names_to = "column", values_to = "q75_value") |>
    dplyr::filter(q75_value != 0 & !is.na(q75_value)) |>
    dplyr::pull(column)

   # Update status (wrapped to prevent crash)
   tryCatch({
    old_item_name <- get_status_name(4, "select_covariates")
     if (!str_detect(old_item_name, "\\[")) {
        new_item_name = paste(old_item_name,"[",i," / ", N,"]")
     } else{
        new_item_name <- str_replace(old_item_name, "\\[([^\\]]*)\\]", paste("[",i," / ", N,"]"))
     }
   }, error = function(e) NULL)

    result  # Return result from tryCatch expression
   },
   error = function(e) {
     warning(sprintf("select_covariates failed: %s. Returning empty character vector.", conditionMessage(e)))
     character(0)
   }
   )

  return(filtered_data)
}
