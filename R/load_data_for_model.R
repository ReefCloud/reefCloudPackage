#' Load Data for Model Execution
#' 
#' Loads benthic survey data and optionally identifies if covariates are available.
#' This function checks for required `.RData` files in the processed data directory,
#' sets up the list of target groups, and flags if covariate data is available.
#' 
#' The following objects are loaded or created in the global environment:
#' \itemize{
#'   \item Dataset from \code{RDATA_FILE} (typically a list of survey or model-ready data).
#'   \item \code{GROUPS} – vector of benthic target groups.
#'   \item \code{RDATA_COV_FILE} – name of the associated file with covariates (if it exists).
#'   \item \code{COVARIATES} – logical flag indicating if covariate file is available.
#'   \item \code{tiers.lookup} – lookup table for tier hierarchy.
#'   \item \code{tier5.sf} – spatial features for Tier5 level.
#'   \item \code{FOCAL_TIER} – character string indicating the modelling tier level (e.g., "Tier4").
#'   \item \code{reef_layer.sf} – spatial reef layer object.
#' }
#' @param DATA_PATH Character. Path to the folder containing `processed/` and `primary/` data directories.
#' @param RDATA_FILE Character. Name of the `.RData` file to load from the `processed/` directory.
#' @return No return value. Loads data into the environment.
#' @examples
#' load_data_for_model()
#' @author Julie Vercelloni
#' @export
load_data_for_model <- function() {
   status::status_try_catch(
   {
  # Load main data file
  rdata_path <- file.path(DATA_PATH, "processed", RDATA_FILE)
  if (file.exists(rdata_path)) {
    load(rdata_path, envir = .GlobalEnv)
  }

  # Set target groups
  assign("GROUPS", c(
    "HARD CORAL",
    "MACROALGAE", "SOFT CORAL"
  ), envir = .GlobalEnv)

  # Check and assign covariate info
  rdata_cov_file <- stringr::str_replace(RDATA_FILE, "_", "_with_covariates")
  assign("RDATA_COV_FILE", rdata_cov_file, envir = .GlobalEnv)

  cov_path <- file.path(DATA_PATH, "processed", rdata_cov_file)
  if (file.exists(cov_path)) {
    assign("COVARIATES", TRUE, envir = .GlobalEnv)
  }

  # Load tier lookup tables
  load(file.path(DATA_PATH, "primary", "tiers.lookup.RData"), envir = .GlobalEnv)
  load(file.path(DATA_PATH, "primary", "tier5.sf.RData"), envir = .GlobalEnv)

  # Define focal tier
  assign("FOCAL_TIER", paste0('Tier', as.numeric(BY_TIER)), envir = .GlobalEnv)

  # Import reef layer 
  load(file=paste0(DATA_PATH, 'primary/reef_layer.sf.RData'), envir = .GlobalEnv)

   },
   stage_ = 4,
   order_ = 1,
   name_ = "Load data",
   item_ = "load_data"
   )
}
