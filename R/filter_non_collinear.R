#' @title Filter non-collinear variables
#' @description Identifies and removes highly correlated variables based on a correlation threshold, while retaining variables with the lowest temporal lag within predefined groups (e.g., cyc and dhw).
#' @param df Data frame containing the variables to evaluate.
#' @param vars Character vector of variable names to test for collinearity.
#' @param threshold Numeric correlation threshold above which variables are considered collinear. Default is 0.7.
#' @return Character vector of selected variables after filtering collinear predictors.
#' @examples
#' selected_vars <- filter_non_collinear(
#'   df = my_data,
#'   vars = c("max_cyc_lag1", "max_cyc_lag3", "max_dhw_lag1"),
#'   threshold = 0.7
#' )
#' @author Julie Vercelloni
#' @export

filter_non_collinear <- function(df, vars, threshold = 0.7) {

  # Need at least 2 variables to check collinearity

  if (length(vars) < 2) return(vars)

  # Extract numeric covariate data (drop geometry if sf)
  if (inherits(df, "sf")) {
    df_numeric <- sf::st_drop_geometry(df)
  } else {
    df_numeric <- as.data.frame(df)
  }

  # Check all vars exist in df

  vars <- intersect(vars, names(df_numeric))
  if (length(vars) < 2) return(vars)

  m_coll <- cor(df_numeric[vars], use = "pairwise.complete.obs")

  corr_long <- as.data.frame(as.table(m_coll)) %>%
    dplyr::filter(Var1 != Var2) %>%
    dplyr::filter(abs(Freq) > threshold) %>%
    dplyr::arrange(dplyr::desc(abs(Freq))) %>%
    dplyr::mutate(group = dplyr::case_when(
      stringr::str_detect(Var1, "cyc") & stringr::str_detect(Var2, "cyc") ~ "max_cyc",
      stringr::str_detect(Var1, "dhw") & stringr::str_detect(Var2, "dhw") ~ "max_dhw",
      TRUE ~ "other"
    ))

  if(nrow(corr_long) == 0) return(vars)

  corr_long <- corr_long %>%
    dplyr::rowwise() %>%
    dplyr::mutate(keep_var = list(select_lowest_lag(as.character(Var1),
                                             as.character(Var2),
                                             group,
                                             group))) %>%
    dplyr::ungroup() %>%
    tidyr::unnest(cols = c(keep_var))

  corr_long <- corr_long %>%
    dplyr::mutate(keep_var = as.character(keep_var),
           Var1 = as.character(Var1),
           Var2 = as.character(Var2))

  non_flagged_vars <- setdiff(vars, unique(c(corr_long$Var1, corr_long$Var2)))

  final_vars <- unique(c(corr_long$keep_var, non_flagged_vars))

  return(final_vars)
}
