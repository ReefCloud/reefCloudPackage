#' @title Check for Rank Deficiency and Update Model Formula
#' @description Checks if the model matrix constructed from selected covariates is rank-deficient (i.e., contains redundant predictors).
#' If rank deficiency is detected, the function computes correlations among covariates and removes the first highly correlated variable.
#' It then re-tests rank and returns the updated formula and status.
#'
#' @param data.grp.tier.ready A data frame with grouped tier-level monitoring data containing COUNT, Tier5, and fYEAR columns.
#' @param HexPred_reefid2 A data frame of covariates (e.g., environmental predictors) with Tier5 and fYEAR for joining.
#' @param selected_covar Character vector of covariate names to include in the model formula.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{status}{A character string: "pass" if the final model matrix is full rank, "fail" otherwise.}
#'   \item{formula}{A character string of the model formula used (with or without one redundant variable removed).}
#' }
#'
#' @author Julie Vercelloni
##' @export
rank_checks <- function(data.grp.tier.ready, HexPred_reefid2, selected_covar) {

  data.grp.tier.ready.avg <- data.grp.tier.ready %>%
    dplyr::group_by(Tier5, fYEAR) %>%
    dplyr::summarize(COUNT = mean(COUNT), .groups = "drop")

  data_with_cov <- data.grp.tier.ready.avg %>%
    dplyr::left_join(HexPred_reefid2 %>% dplyr::mutate(fYEAR = as.factor(fYEAR)), 
                     by = c("fYEAR", "Tier5"))

  if (length(selected_covar) > 0){
  formula_no_random <- paste("COUNT ~ 1 +", paste(selected_covar, collapse = " + "))
  } else{
  formula_no_random <- "COUNT ~ 1"
  }
  X_O <- model.matrix(as.formula(formula_no_random), data = data_with_cov)

  # Initial full-rank check
  if (qr(X_O)$rank < ncol(X_O)) {

    # Correlation check
    data_with_cov_formula <- data_with_cov %>%
      dplyr::ungroup() %>%
      dplyr::select(all_of(selected_covar))

    cor_matrix <- cor(data_with_cov_formula[, sapply(data_with_cov_formula, is.numeric)])
    threshold <- 0.99
    cor_mat_ut <- cor_matrix
    cor_mat_ut[lower.tri(cor_mat_ut, diag = TRUE)] <- NA

    high_cor_indices <- which(abs(cor_mat_ut) >= threshold, arr.ind = TRUE)
    if (length(high_cor_indices) == 0) {
      return(list(status = "fail", formula = formula_no_random))
    }

    high_cor_pairs <- apply(high_cor_indices, 1, function(idx) {
      list(
        var1 = rownames(cor_mat_ut)[idx[1]],
        var2 = colnames(cor_mat_ut)[idx[2]],
        correlation_value = cor_mat_ut[idx[1], idx[2]]
      )
    })

    remove_var <- high_cor_pairs[[1]]$var1
    formula_cleaned <- reefCloudPackage::update_formula(remove_var, formula_no_random)

    X_O_1 <- model.matrix(as.formula(formula_cleaned), data = data_with_cov)

    if (qr(X_O_1)$rank < ncol(X_O_1)) {
      return(list(status = "fail", formula = formula_cleaned))
    } else {
      return(list(status = "pass", formula = formula_cleaned))
    }

  } else {
    return(list(status = "pass", formula = formula_no_random))
  }
}
