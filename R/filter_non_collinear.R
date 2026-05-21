 
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
  
  m_coll <- cor(df[vars], use = "pairwise.complete.obs")
  
  corr_long <- as.data.frame(as.table(m_coll)) %>%
    filter(Var1 != Var2) %>%
    filter(abs(Freq) > threshold) %>%
    arrange(desc(abs(Freq))) %>%
    mutate(group = case_when(
      str_detect(Var1, "cyc") & str_detect(Var2, "cyc") ~ "max_cyc",
      str_detect(Var1, "dhw") & str_detect(Var2, "dhw") ~ "max_dhw",
      TRUE ~ "other"
    ))

  if(nrow(corr_long) == 0) return(vars)
  
  corr_long <- corr_long %>%
    rowwise() %>%
    mutate(keep_var = list(select_lowest_lag(as.character(Var1),
                                             as.character(Var2),
                                             group,
                                             group))) %>%
    ungroup() %>%
    unnest(cols = c(keep_var))
  
  corr_long <- corr_long %>%
    mutate(keep_var = as.character(keep_var),
           Var1 = as.character(Var1),
           Var2 = as.character(Var2))
  
  non_flagged_vars <- setdiff(vars, unique(c(corr_long$Var1, corr_long$Var2)))
  
  final_vars <- unique(c(corr_long$keep_var, non_flagged_vars))
  
  return(final_vars)
}
