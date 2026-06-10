#' @title Fit model
#' @description 
#' Fits the appropriate statistical model to benthic data, depending on the domain category.
#' If `DOMAIN_CATEGORY` is "site", it runs the site-level model; otherwise, it fits the tier-level model.
#' Optionally runs raw or smoothed cell means based on the `raw_cell_means` flag.
#' @param raw_cell_means Logical. If TRUE, fits raw cell means model. If FALSE, fits tier-level model. Default is TRUE.
#'
#' @examples 
#' # Fit the model using raw cell means
#' model_fitModel(raw_cell_means = TRUE)
#' @author Murray Logan
#' @export
model_fitModel <- function(raw_cell_means = TRUE){

  status::status_set_stage(stage = 4, title = "Model data")

  ## There are sufficient differences in the processing of benthic data
  ## between 'site' and 'tier' level analyses to warrent forking off the
  ## code into separate paths at this point.

  if (!DEBUG_MODE) cli::cli_h1("Model fitting")

  if (DOMAIN_CATEGORY == "site") {
    reefCloudPackage::model_fitModelSite(raw_cell_means)
  } else {
    reefCloudPackage::model_fitModelTier() 
    if (GENERATE_REPORT) reefCloudPackage::model_summariseModelTier()
  }

}
