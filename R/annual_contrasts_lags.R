#' @title Fit model at tier level
#' @title Annual Contrasts with Lags Matrix
#' @description Constructs a matrix representing annual contrasts with specified lags for a vector of values.
#' Each column corresponds to a contrast between year i and year i + lag.
#' 
#' @param x A vector of values (e.g., years or time points).
#' @param lags Integer or vector of integers indicating the lag steps between values for contrasts. Default is 1.
#' 
#' @return A matrix with rows corresponding to input values and columns to contrasts between lagged pairs.
#' 
#' @examples
#' years <- 2010:2015
#' annual_contrasts_lags(years, lags = c(1,2))
#' 
#' @author Murray Logan
#' @export
annual_contrasts_lags <- function(x, lags = 1) {

  k <- length(x)
  M <- matrix(0, nrow = k, ncol = k * length(lags))
  nm <- vector("character", k * length(lags))
  
  for (i in seq_len(k - 1)) {
    for (j in seq_along(lags)) {
      lag <- lags[j]
      col_index <- (j - 1) * k + i
      
      if (i + lag > k) {
        next  # Skip invalid lags beyond vector length
      }
      
      M[i, col_index] <- 1
      M[i + lag, col_index] <- -1
      nm[col_index] <- paste(x[i], x[i + lag], sep = " - ")
    }
  }
  
  # Remove unused columns (all zeros)
  valid_cols <- colSums(abs(M)) > 0
  M <- M[, valid_cols, drop = FALSE]
  nm <- nm[valid_cols]
  
  rownames(M) <- x
  colnames(M) <- nm
  
  return(as.matrix(M))
}
