
## simpleINLA -----------------------------------------------------------------------

#' @title Function
#' @description Description
#' @param parameters description
#' @return returned arguments description
#' @examples examples
#' @export
simpleINLA <- function(data.sub, GROUP, TIER) {
  ## reefCloudPackage::ReefCloud_tryCatch({
    ## ---- simpleINLA
    {
      cli::cli_progress_bar("Simple INLA cellmeans", type = "iterator", total = 7, clear = TRUE)
      reefCloudPackage::log('INFO', logFile = LOG_FILE,
                    Category = '--Modelling (Simple INLA)--',
                    msg = paste0(DOMAIN_NAME, ": ", GROUP, " Simple INLA cellmeans calculated"))

      .data <- reefCloudPackage::simpleINLA_data(data.sub, GROUP, TIER)
      cli::cli_progress_update(force=TRUE)

      .data.fit <- simpleINLA_fit(data.sub, GROUP, TIER, .data = .data)
      cli::cli_progress_update(force=TRUE)

      .data.fit.cm <- reefCloudPackage::simpleINLA_tier5a(data.sub, GROUP, TIER, .data = .data, .data.fit = .data.fit)
      cli::cli_progress_update(force=TRUE)

      .data.tier5 <- reefCloudPackage::simpleINLA_tier5(data.sub, GROUP, TIER, .data = .data, .data.fit.cm = .data.fit.cm)
      cli::cli_progress_update(force=TRUE)

      ## .data.tier4 <- reefCloudPackage::simpleINLA_tier4(data.sub, GROUP, TIER, .data.tier5 = .data.tier5)
      ## cli::cli_progress_update(force=TRUE)

      ## .data.tier3 <- reefCloudPackage::simpleINLA_tier3(data.sub, GROUP, TIER, .data.tier5 = .data.tier5)
      ## cli::cli_progress_update(force=TRUE)

      ## .data.tier2 <- reefCloudPackage::simpleINLA_tier2(data.sub, GROUP, TIER, .data.tier5 = .data.tier5)
      ## cli::cli_progress_update(force=TRUE)
    }
    ## ----end
  ## }, logFile=LOG_FILE, Category='--Modelling fitting routines--',
  ## msg=paste0('Simple INLA cell means for ', stringr::str_to_title(GROUP), ' Tier ', TIER), return=NULL)
}
