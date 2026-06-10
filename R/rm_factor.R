## rm_factor -----------------------------------------------------------------------

#' @title Remove Factor Variables with Fewer than Two Levels
#'
#' @description
#' Cleans a model formula by removing factor variables that have fewer than two levels.
#' Continuous (numeric) variables are retained. Also removes random effect terms of the form \code{f(var)}.
#'
#' @param model_formula A model formula containing fixed and random effects.
#' @param data.sub A data frame with the variables used in the model.
#'
#' @return A cleaned model formula excluding low-level factors.
#'
#' @examples
#' \dontrun{
#' data.sub <- data.frame(
#'   y = rnorm(10),
#'   factor1 = factor(rep("A", 10)),
#'   factor2 = factor(rep(c("A", "B"), 5)),
#'   numeric1 = rnorm(10)
#' )
#' model_formula <- y ~ factor1 + factor2 + numeric1 + f(factor2, model = 'iid')
#' rm_factor(model_formula, data.sub)
#' }
#' @author Murray Logan
#' @export
rm_factor <- function(model_formula, data.sub) {
  term.labels <- attr(terms(model_formula), "term.labels")
  vars <- all.vars(model_formula)[-1]
  response_var <- all.vars(model_formula)[1]

  for (v in vars) {
    v_data <- data.sub[[v]]
    if (is.numeric(v_data)) next
    if (is.factor(v_data) && length(levels(v_data)) >= 2) next
    term.labels <- term.labels[term.labels != v]
    term.labels <- term.labels[!grepl(paste0("^f\\(", v, "\\b"), term.labels)]
  }

  model_formula <- reformulate(term.labels, response = response_var)

  # Add intercept back in if only random effects remain
  if (all(stringr::str_detect(term.labels, "^f\\(")))
    model_formula <- update(model_formula, . ~ . + 1)

  return(model_formula)
}
