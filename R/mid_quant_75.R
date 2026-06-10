#' Compute the 75th Mid-Quantile of a Numeric Vector
#'
#' Computes the 75th percentile (mid-quantile) of a numeric vector using the 
#' `midquantile()` function from the `Qtools` package.
#' The mid-quantile is useful for skewed data because it smooths
#' the empirical cumulative distribution function.
#'
#' @param y A numeric vector.
#' @return A single numeric value representing the 75th mid-quantile of `y`.
#' @author Julie Vercelloni
#' @examples
#' \dontrun{
#' library(Qtools)
#' x <- c(0, 0, 1, 3, 5)
#' mid_quant_75(x)
#' }
#' @export

mid_quant_75 <- function(y){
  y <- as.numeric(as.character(y))
  y <- y[!is.na(y)]
  # Return NA if insufficient non-NA values for interpolation
  if (length(y) < 2) return(NA_real_)

  # Try midquantile, return NA on error (e.g., interpolation issues)
  tryCatch({
    ymid <- Qtools::midquantile(y, probs = 3/4)
    return(ymid$y)
  }, error = function(e) {
    return(NA_real_)
  })
}
