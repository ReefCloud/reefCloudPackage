#' Load Predictive Layers with Covariates
#'
#' Loads spatial prediction layers containing environmental covariates for Tier 5.
#'
#' @title Load Predictive Layers
#' @param NULL No arguments required.
#' @return A data object `full_cov_raw`, typically a `sf` or data frame with covariates.
#' @examples
#' \dontrun{
#' full_cov <- load_predictive_layers()
#' }
#' @author Julie Vercelloni
#' @export
load_predictive_layers <- function(i , N) {
   status::status_try_catch(
   {
  files <- list.files(path = paste0(DATA_PATH, "processed"),
                      pattern = "covariates_full_tier5.RData", full.names = TRUE)

  if (file.exists(files)) {
    full_cov_raw <- get(load(files))

    # CRITICAL: Validate that loaded data is sf object
    cat(paste0("\n=== DEBUG: load_predictive_layers() output ===\n"))
    cat(paste0("File loaded: ", files, "\n"))
    cat(paste0("Class: ", paste(class(full_cov_raw), collapse = ", "), "\n"))
    cat(paste0("Is sf: ", inherits(full_cov_raw, "sf"), "\n"))
    cat(paste0("Rows: ", nrow(full_cov_raw), "\n"))
    cat(paste0("Columns: ", ncol(full_cov_raw), "\n"))

    if (!inherits(full_cov_raw, "sf")) {
      warning("CRITICAL: covariates_full_tier5.RData does not contain an sf object! ",
              "It contains: ", paste(class(full_cov_raw), collapse = ", "))
      stop("Predictive layers must be sf objects for spatial modeling. ",
           "Check data processing pipeline that creates covariates_full_tier5.RData")
    }

    cat("Validation passed - full_cov_raw is an sf object\n\n")
  } else {
    stop("Predictive layers not found")
  }

   # Update status 
    old_item_name <- get_status_name(4, "load_predictive_layers")
     if (!str_detect(old_item_name, "\\[")) {
        new_item_name = paste(old_item_name,"[",i," / ", N,"]")
     } else{
        new_item_name <- str_replace(old_item_name, "\\[([^\\]]*)\\]", paste("[",i," / ", N,"]"))
     }
    ## ##  status:::update_status_name (  # Disabled - private function not exportedstage = 4, item = "load_predictive_layers", name = new_item_name)  # Disabled - private function
   },
   stage_ = 4,
   name_ = "Load predictive layers",
   item_ = "load_predictive_layers"
   )
  return(full_cov_raw)
}
