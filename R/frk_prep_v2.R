#' Prepare objects for the FRK model
#'
#' @title frk_prep
#' @param data.grp.tier Data frame with observations for model fitting, including columns: LONGITUDE, LATITUDE, fYEAR, TOTAL, etc.
#' @param HexPred_reefid2 Spatial polygons with covariates and reef IDs
#' @return A list with prepared objects for FRK: ST_BAUs, STObj, basis functions
#' @examples
#' \dontrun{
#' # Example usage:
#' obj <- frk_prep(data.grp.tier = my_data, HexPred_reefid2 = my_spatial_covariates)
#' plot(obj$basis)
#' }
#' @author Julie Vercelloni
#' @export
frk_prep_v2 <- function(data.grp.tier, HexPred_reefid2, i, N, basis_resolution = 3L) {
  status::status_try_catch(
    {
      # Convert fYEAR to Date (start of year)

      # DEFENSIVE: Ensure data.grp.tier is a base data.frame (not a tibble)
      if (inherits(data.grp.tier, c("tbl_df", "tbl"))) {
        cat("  WARNING: data.grp.tier is a tibble, converting to data.frame\n")
        data.grp.tier <- as.data.frame(data.grp.tier)
      }

      data.grp.tier$Year <- as.Date(paste0(as.character(data.grp.tier$fYEAR), "-01-01"))
      data.grp.tier$k_Z <- data.grp.tier$TOTAL # number of trials

      lon_idx <- which(names(data.grp.tier) == "LONGITUDE")
      lat_idx <- which(names(data.grp.tier) == "LATITUDE")

      STObj <- stConstruct(
        x = data.grp.tier,
        space = c(lon_idx, lat_idx),
        time = "Year",
        interval = TRUE
      )

      # Convert HexPred_reefid2 to sp object for BAU construction
      HexPred_sp <- as_Spatial(HexPred_reefid2)
      nHEX <- nrow(subset(HexPred_sp, fYEAR == min(HexPred_sp@data$fYEAR)))
      nYEAR <- length(unique(HexPred_sp@data$fYEAR))

      HexPred_sp@data$n_spat <- rep(1:nHEX, each = nYEAR)
      BAUs_spat <- subset(HexPred_sp, fYEAR == min(HexPred_sp@data$fYEAR))
      coordnames(BAUs_spat) <- c("LONGITUDE", "LATITUDE")

      ST_BAUs <- FRK::auto_BAUs(
        manifold = STplane(),
        data = STObj,
        spatial_BAUs = BAUs_spat,
        tunit = "years"
      )

      ST_BAUs <- ST_BAUs[, 1:nYEAR, 1:2] # Remove extra year
      ST_BAUs$fYEAR <- as.character(ST_BAUs$t + (min(as.numeric(HexPred_sp@data$fYEAR)) - 1))
      ST_BAUs$n_spat <- rep(1:nHEX, nYEAR)

      ST_BAUs@data$fYEAR <- as.integer(ST_BAUs@data$fYEAR)
      ST_BAUs@data <- dplyr::left_join(ST_BAUs@data, HexPred_sp@data, by = c("fYEAR", "n_spat"))

      ST_BAUs$fs <- 1
      ST_BAUs@sp@proj4string <- CRS()

      ST_BAUs@data$reefid <- as.factor(as.character(ST_BAUs@data$reefid))
      ST_BAUs@data$yearid <- as.factor(ST_BAUs@data$fYEAR)

      # Remove overlapping covariates from STObj
      overlapping_fields <- intersect(names(STObj@data), names(ST_BAUs@data))
      STObj@data[, overlapping_fields] <- NULL

      # Create basis functions
      basis <- FRK::auto_basis(STplane(),
        ST_BAUs,
        tunit = "years",
        nres = basis_resolution,
        regular = TRUE
      )

      #  Update status (wrapped in tryCatch since get_status_name may not exist in test env)
      tryCatch(
        {
          old_item_name <- get_status_name(4, "prep_FRK_objects")
          if (!stringr::str_detect(old_item_name, "\\[")) {
            new_item_name <- paste(old_item_name, "[", i, " / ", N, "]")
          } else {
            new_item_name <- stringr::str_replace(old_item_name, "\\[([^\\]]*)\\]", paste("[", i, " / ", N, "]"))
          }
          ## ##  status:::update_status_name (  # Disabled - private function not exportedstage = 4, item = "prep_INLA_objects", name = new_item_name)  # Disabled - private function
        },
        error = function(e) {
          # Ignore status update errors (function may not exist in test environment)
          NULL
        }
      )
    },
    stage_ = 4,
    order_ = 9,
    name_ = "Prep FRK objects",
    item_ = "prep_FRK_objects"
  )

  obj_frk <- list("ST_BAUs" = ST_BAUs, "STObj" = STObj, "basis" = basis)
  return(obj_frk)
}
