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
  # Load main data file - prefer covariate version if it exists
  rdata_cov_file <- stringr::str_replace(RDATA_FILE, "\\.RData$", "_with_covariates.RData")
  rdata_cov_path <- file.path(DATA_PATH, "processed", rdata_cov_file)
  rdata_path <- file.path(DATA_PATH, "processed", RDATA_FILE)

  if (file.exists(rdata_cov_path)) {
    # Load covariate version (from Stage 3 checkpoint or fresh run)
    load(rdata_cov_path, envir = .GlobalEnv)
  } else if (file.exists(rdata_path)) {
    # Fall back to base version (from Stage 2 checkpoint)
    load(rdata_path, envir = .GlobalEnv)
  }

  # Set target groups
  assign("GROUPS", c(
    "CRUSTOSE CORALLINE ALGAE", "HARD CORAL",
    "MACROALGAE", "TURF ALGAE", "SOFT CORAL"
  ), envir = .GlobalEnv)

  # Check and assign covariate info
  rdata_cov_file <- stringr::str_replace(RDATA_FILE, "\\.RData$", "_with_covariates.RData")
  assign("RDATA_COV_FILE", rdata_cov_file, envir = .GlobalEnv)

  cov_path <- file.path(DATA_PATH, "processed", rdata_cov_file)
  if (file.exists(cov_path)) {
    assign("COVARIATES", TRUE, envir = .GlobalEnv)
  } else {
    assign("COVARIATES", FALSE, envir = .GlobalEnv)
  }

  # Load tier lookup tables
  load(file.path(DATA_PATH, "primary", "tiers.lookup.RData"), envir = .GlobalEnv)

  # tier5.sf.RData stores the object as 'tier.sf' — rename to 'tier5.sf'
  # for downstream code while keeping 'tier.sf' for model function calls
  load(file.path(DATA_PATH, "primary", "tier5.sf.RData"), envir = .GlobalEnv)
  if (exists("tier.sf", envir = .GlobalEnv) && !exists("tier5.sf", envir = .GlobalEnv)) {
    assign("tier5.sf", get("tier.sf", envir = .GlobalEnv), envir = .GlobalEnv)
  }

  # Define focal tier
  assign("FOCAL_TIER", paste0('Tier', as.numeric(BY_TIER)), envir = .GlobalEnv)

  # Import reef layer 
  load(file=paste0(DATA_PATH, 'primary/reef_layer.sf.RData'), envir = .GlobalEnv)

   },
   stage_ = 4,
   name_ = "Load data",
   item_ = "load_data"
   )
}
