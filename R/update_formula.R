#' @title Remove a variable from model formula
#' @description Automatically removes a specified covariate from a model formula string by matching typical additive patterns.
#' @param remove_var Character name of the variable to remove from the formula.
#' @return A cleaned model formula string with the specified variable removed and formatting adjusted.
#' @author Julie Vercelloni
#' @export
update_formula <- function(remove_var, formula_no_random) {
  
  # Pattern matches:
  # - "+ max_cyc_lag2"
  # - "max_cyc_lag2 +"
  # - "max_cyc_lag2" if alone
  pattern <- paste0(
    "(\\+\\s*", remove_var, "\\b)|",
    "(\\b", remove_var, "\\s*\\+)|",
    "(\\b", remove_var, "\\b)"
  )

  formula_cleaned <- str_replace_all(formula_no_random, pattern, function(x) {
    if (str_detect(x, "^\\+")) {
      return(" + ")
    } else if (str_detect(x, "\\+$")) {
      return(" + ")
    } else {
      return("")
    }
  })

  # Clean up any formatting leftovers
  formula_cleaned <- str_replace_all(formula_cleaned, "\\+\\s*\\+", "+")
  formula_cleaned <- str_replace(formula_cleaned, "\\+\\s*$", "")
  formula_cleaned <- str_replace(formula_cleaned, "(~)\\s*\\+", "\\1 ")
  formula_cleaned <- str_trim(formula_cleaned)

  return(formula_cleaned)
}