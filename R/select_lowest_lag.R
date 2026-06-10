#' @title Select variable with lowest lag
#' @description Compares two correlated variables within the same predictor group and retains the variable with the lowest temporal lag. If variables belong to different groups, both are retained.
#' @param var1 Character string representing the first variable name.
#' @param var2 Character string representing the second variable name.
#' @param group1 Character string indicating the group of the first variable.
#' @param group2 Character string indicating the group of the second variable.
#' @return A character vector containing the selected variable(s).
#' @examples
#' select_lowest_lag(
#'   var1 = "max_cyc_lag1",
#'   var2 = "max_cyc_lag3",
#'   group1 = "max_cyc",
#'   group2 = "max_cyc"
#' )
#' @author Julie Vercelloni
#' @export

select_lowest_lag <- function(var1, var2, group1, group2) {
    if(group1 == group2 && group1 %in% c("max_cyc", "max_dhw")) {
      lag1 <- ifelse(str_detect(var1, "lag"), as.numeric(str_extract(var1, "\\d+")), 0)
      lag2 <- ifelse(str_detect(var2, "lag"), as.numeric(str_extract(var2, "\\d+")), 0)
      if (lag1 <= lag2) return(var1) else return(var2)
    } else {
      # Keep both if not in same group
      return(c(var1, var2))
    }
  }
