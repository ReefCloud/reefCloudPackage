#' @title FRK with precision matrix regularization
#' @description Wraps FRK::FRK() with a namespace patch that adds a ridge
#'   (epsilon * I) to the spatial precision matrix diagonal before Cholesky
#'   decomposition. This prevents singular matrix errors when spatial coverage
#'   is sparse relative to the number of basis functions.
#'
#' @details The patch intercepts FRK's internal `.TMB_data_prep()` function
#'   via namespace manipulation. For K_type == "neighbour" (which is what
#'   K_type = "precision" resolves to for regular grids), the diagonal entries
#'   of the precision template matrix `x` are augmented by `ridge`.
#'
#'   The original function is always restored via on.exit(), even if FRK::FRK()
#'   throws an error.
#'
#'   When ridge == 0 (default), FRK::FRK() is called directly with no patching.
#'
#' @param ridge Numeric ridge value added to precision matrix diagonal.
#'   Default 0 (disabled). Recommended value when enabled: 1e-6.
#'   Only applies when K_type resolves to "neighbour" internally.
#' @param ... All arguments passed through to FRK::FRK()
#' @return FRK model object (same as FRK::FRK() return value)
#' @examples
#' # Without regularization (default / original behavior):
#' # FRK_regularized(f = formula, data = ..., ridge = 0)
#'
#' # With regularization:
#' # FRK_regularized(f = formula, data = ..., ridge = 1e-6)
#' @export
FRK_regularized <- function(..., ridge = 0) {

  if (!is.numeric(ridge) || length(ridge) != 1 || is.na(ridge) || ridge < 0) {
    ridge <- 0
  }

  if (ridge == 0) {
    return(FRK::FRK(...))
  }

  # Patch FRK's internal .TMB_data_prep to add ridge to precision diagonal
  ns <- asNamespace("FRK")
  original_fn <- get(".TMB_data_prep", envir = ns)

  patched_fn <- function(...) {
    data <- original_fn(...)
    if (identical(data$K_type, "neighbour")) {
      diag_idx <- which(data$row_indices == data$col_indices)
      data$x[diag_idx] <- data$x[diag_idx] + ridge
      cat(sprintf(
        "  [REGULARIZE] Added ridge=%.2e to %d diagonal entries of precision matrix\n",
        ridge, length(diag_idx)
      ))
    }
    data
  }

  unlockBinding(".TMB_data_prep", ns)
  assign(".TMB_data_prep", patched_fn, envir = ns)
  lockBinding(".TMB_data_prep", ns)
  on.exit({
    unlockBinding(".TMB_data_prep", ns)
    assign(".TMB_data_prep", original_fn, envir = ns)
    lockBinding(".TMB_data_prep", ns)
  }, add = TRUE)

  FRK::FRK(...)
}
