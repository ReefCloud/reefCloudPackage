  extract_lags_from_colnames <- function(contrast_matrix) {
    ## Get column names
    col_names <- colnames(contrast_matrix)
    ## Extract lags by parsing the column names
    lags <- sapply(col_names, function(name) {
      ## Split the column name into two years
      years <- as.numeric(unlist(strsplit(name, " - ")))
      ## Compute the lag as the difference between the years
      if (length(years) == 2) {
        return(abs(diff(years)))
      } else {
        return(NA)  # Handle invalid column names gracefully
      }
    })
    return(lags)
  }