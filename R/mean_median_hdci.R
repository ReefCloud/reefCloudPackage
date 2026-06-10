

##########################################################################
## The following function calculates mean, median and HPD intervals     ##
## from posteriors:                                                     ##
## Arguments:                                                           ##
##    - .data: a dataframe or tibble                                    ##
##    - ....:  other arguments pass on to ggdist::mean_hdci() and       ##
##             ggdist::median_hdci()                                    ##
## Returns:                                                             ##
##    - a tibble containing mean, median, lower and upper HPD intervals ##
##########################################################################
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
mean_median_hdci <- function(.data, ...) {
    col_exprs <- rlang::quos(..., .named = TRUE)
    col_expr = col_exprs[[1]]
    col_name = names(col_exprs)
    x1 <- .data %>%
        ggdist::mean_hdci(!!rlang::sym(col_name), na.rm=TRUE)
    x2 <- .data %>%
        ggdist::median_hdci(!!rlang::sym(col_name), na.rm=TRUE)
    x1 %>%
        dplyr::rename(mean = value, lower = .lower, upper = .upper) %>%
        dplyr::select(-.point, -.width, -.interval) %>%
        dplyr::bind_cols(x2 %>%
                  dplyr::ungroup() %>%
                  dplyr::select(median = value))
}
