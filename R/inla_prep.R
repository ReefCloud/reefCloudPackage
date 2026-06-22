# INLAprep -----------------------------------------------------------------------

#' @title Prepare INLA Observation and Prediction Stacks
#' @description
#' Creates the observation (`stk.obs`) and prediction (`stk.pred`) stacks
#' required for fitting tier-level INLA models. The function prepares
#' observation data, prediction covariates, temporal indices, random-effect
#' identifiers, and combines both stacks into a single INLA stack object.
#'
#' @param data.grp.tier.ready Data frame containing observations and response
#'   variables for the current tier.
#' @param HexPred_reefid2 Data frame or sf object containing prediction
#'   covariates at the Tier5-year level.
#' @param i Integer indicating the current tier iteration.
#' @param N Integer indicating the total number of tiers being processed.
#'
#' @return A list containing:
#' \describe{
#'   \item{data.sub}{Prepared observation dataset used to build the observation stack.}
#'   \item{stk.full}{Combined INLA stack containing both observation and prediction stacks.}
#' }
#'
#' @examples
#' obj_inla <- inla_prep(
#'   data.grp.tier.ready = data.grp.tier.ready,
#'   HexPred_reefid2 = HexPred_reefid2,
#'   i = 1,
#'   N = 1
#' )
#' @author Julie Vercelloni
#' @export
inla_prep <- function(data.grp.tier.ready, HexPred_reefid2, i, N) {
  status::status_try_catch(
    {
      #   save_and_upload_debug <- function(data, filename_base, stage_num, focal_tier = "unknown") {
      #   debug_dir <- paste0(DATA_PATH, "debug/inla_prep/")
      #   dir.create(debug_dir, showWarnings = FALSE, recursive = TRUE)
      #   timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

      #   # Create explanatory filename
      #   full_filename <- paste0(
      #     "debug_inla_prep_",
      #     sprintf("%02d", stage_num), "_",
      #     filename_base, "_",
      #     focal_tier, "_", TIER, "_",
      #     timestamp,
      #     ".csv"
      #   )

      #   local_path <- file.path(debug_dir, full_filename)

      #   # Save CSV locally
      #   write.csv(data, local_path, row.names = FALSE)

      #   # Upload to S3 with compression
      #   s3_key <- paste0("inla_prep/", full_filename)
      #   upload_debug_to_s3(local_path, s3_key, compress = TRUE)
      # }

      # # Silent success marker (debug disabled)
      # status::status_try_catch(
      #   invisible(NULL),
      #   stage_ = 4,
      #   name_ = "Debug input dump",
      #   item_ = "debug_input"
      # )

      ######### Prepare stk.obs
      data.sub <- left_join(data.grp.tier.ready, HexPred_reefid2) %>%
        dplyr::select(
          P_CODE, REEF, SITE_NO, TRANSECT_NO, LONGITUDE, LATITUDE, Tier2:Tier5, fYEAR, fDEPTH, fGROUP:TOTAL,
          dplyr::starts_with("max_cyc"),
          dplyr::starts_with("max_dhw")
        ) %>%
        dplyr::mutate(
          fYEAR = as.factor(fYEAR),
          fDEPTH = as.factor(fDEPTH),
          Site = as.factor(REEF),
          Transect = as.factor(interaction(REEF, TRANSECT_NO))
        ) %>%
        dplyr::mutate(year_id = as.integer(factor(fYEAR,
          levels = sort(unique(fYEAR))
        ))) %>%
        droplevels()

      stk.obs <- INLA::inla.stack(
        data = list(y = data.sub$COUNT, TOTAL = data.sub$TOTAL),
        A = list(1),
        effects = list(
          data.frame(
            b0 = 1,
            year_id = data.sub$year_id,
            Transect = data.sub$Transect,
            Site = data.sub$Site,
            max_cyc = data.sub$max_cyc,
            max_cyc.lag1 = data.sub$max_cyc.lag1,
            max_cyc.lag2 = data.sub$max_cyc.lag2,
            max_dhw = data.sub$max_dhw,
            max_dhw.lag1 = data.sub$max_dhw.lag1,
            max_dhw.lag2 = data.sub$max_dhw.lag2
          )
        ),
        tag = "obs"
      )

      ######### Prepare stk.pred
      HexPred_reefid2 <- HexPred_reefid2 %>%
        data.frame() %>%
        dplyr::mutate(
          year_id = as.integer(factor(fYEAR,
            levels = sort(unique(data.sub$fYEAR))
          ))
        ) %>%
        droplevels()

      stk.pred <- INLA::inla.stack(
        data = list(y = NA),
        A = list(1),
        effects = list(
          data.frame(
            b0 = 1,
            year_id = HexPred_reefid2$year_id,
            Transect = NA,
            Site = NA,
            max_cyc = HexPred_reefid2$max_cyc,
            max_cyc.lag1 = HexPred_reefid2$max_cyc.lag1,
            max_cyc.lag2 = HexPred_reefid2$max_cyc.lag2,
            max_dhw = HexPred_reefid2$max_dhw,
            max_dhw.lag1 = HexPred_reefid2$max_dhw.lag1,
            max_dhw.lag2 = HexPred_reefid2$max_dhw.lag2
          )
        ),
        tag = "pred"
      )

      ######### Combine

      stk.full <- INLA::inla.stack(stk.obs, stk.pred)

      #  Update status (wrapped in tryCatch since get_status_name may not exist in test env)
      tryCatch(
        {
          old_item_name <- get_status_name(4, "prep_INLA_objects")
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
    order_ = 13,
    name_ = "Prep INLA objects",
    item_ = "prep_INLA_objects"
  )
  # Return results
  return(list(data.sub = data.sub, stk.full = stk.full))
}
