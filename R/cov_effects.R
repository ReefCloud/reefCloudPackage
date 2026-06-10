#' Scale Up Predictions from Model Outputs
#'
#' This function reads model outputs (either type5 or other specified models), processes them
#' to scale up predictions across different tiers, and saves the summarised results.
#'
#' @param whichModel Character string indicating the model type (e.g., "type5").
#' @return This function has no return value. It writes summarised prediction CSV files to disk.
#' @examples
#' scale_up_pred("type5")
#' @export
cov_effects <- function() {

# Read files
# Extract M model
# functions is INLA or FRK
# create table
# send to AWS path 

  # CASE 1: FRK/INLA model output (type5/type6)
  if (whichModel %in% c("type5", "type6")) {
    files <- list.files(
      path = paste0(DATA_PATH, "modelled"),
      pattern = "FRK|INLA", 
      full.names = TRUE
    )
    files <- files[!grepl('TIER', files, perl = TRUE)]
    data.list <- vector('list', length(files))
    post_dist_df_list <- list()

}  

}