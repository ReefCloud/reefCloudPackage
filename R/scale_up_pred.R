#' Scale Up Predictions from Model Outputs
#'
#' This function reads model outputs (either type6 or other specified models), processes them
#' to scale up predictions across different tiers, extract covariate effect sizes, compute annual contrasts and saves the summarised results.
#'
#' @param whichModel Character string indicating the model type (e.g., "type6").
#' @return This function has no return value. It writes summarised prediction CSV files to AWS path.
#' @examples
#' scale_up_pred("type6")
#' @author Julie Vercelloni
#' @export
scale_up_pred <- function(whichModel) {

  status::status_try_catch({

    # ---- Load input data tables for modelling ----
   # reefCloudPackage::load_data_for_model()
  #  load(file.path(DATA_PATH, "primary", "tier5.sf.RData"), envir = .GlobalEnv)
  
  # ---- CASE 1: FRK/INLA model output (type5/type6) ----
    if (whichModel %in% c("type5", "type6")) {
      
      files <- list.files(
        path = paste0(DATA_PATH, "modelled"),
        pattern = "FRK|INLA", 
        full.names = TRUE
      )
      files <- files[!grepl('TIER', files, perl = TRUE)]

      # Stop if no model outputs found
      if (length(files) == 0) {
        msg <- paste("No model outputs for the region")
        status:::status_log("ERROR", log_file = log_file, "--Model predictions--", msg = msg)
        stop("No model outputs found")
      }

      # ---- Load and prepare all model outputs ----
      data.list <- vector('list', length(files))
      post_dist_df_list <- list()
      data_tier_list <- list()

      for (i in seq_along(data.list)) {
        GROUP <- "HARD CORAL"
        tier <- stringr::str_extract(files[i], "(?<=_)(\\d+)(?=.RData)")
        obj <- readRDS(files[i])

        # Temporary renaming for compatibility
        if ("data.sub" %in% names(obj)) {
          names(obj)[names(obj) == "data.sub"] <- "data.grp.tier"
        }

        post_dist_df_list[[i]] <- obj$post_dist_df
        data_tier_list[[i]] <- unique(obj$data.grp.tier$Tier5) 
      }

      # Add tier type variable
      for (i in seq_along(post_dist_df_list)) {
        post_dist_df_list[[i]] <- post_dist_df_list[[i]] |>
          dplyr::mutate(
            tier_type = ifelse(
              as.character(Tier5) %in% data_tier_list[[i]],
              "data", "new"
            )
          )
      }

      rm(data.list)

      # Keep only valid elements
      post_dist_df_list <- post_dist_df_list |> purrr::keep(~ "model_name" %in% names(.x))

      # Format data types and structure
      post_dist_df_list <- map(post_dist_df_list, ~ .x |>
        dplyr::mutate(
          fYEAR = as.factor(fYEAR),
          Tier5 = as.factor(Tier5),
          id_loc = as.integer(id_loc),
          draw = as.character(draw),
          pred = as.numeric(pred),
          model_name = as.character(model_name),
          tier_type = as.character(tier_type)
        ) |>
        dplyr::select(fYEAR, Tier5, id_loc, draw, pred, model_name, tier_type)
      )
    
    load(file.path(DATA_PATH, "primary", "tiers.lookup.RData"), envir = .GlobalEnv)

      # Bind and weight all tiers
      post_dist_df_all <- dplyr::bind_rows(post_dist_df_list) %>%
        dplyr::left_join(tiers.lookup) %>%
        dplyr::mutate(
          reef_area = reef_area / 1000000,
          weighted_pred = pred * reef_area
        )
    
      # Bind all Tier5 rows
      post_dist_df_tier5 <- dplyr::bind_rows(post_dist_df_list) %>%
        dplyr::left_join(tiers.lookup)

      rm(post_dist_df_list)

      # Log warnings for NAs
      if (anyNA(post_dist_df_tier5) || anyNA(post_dist_df_all)) {
        msg <- "Some model outputs contain NA values. Possibly not saved in the correct folder."
        status:::status_log("WARNING", log_file = log_file, "--Model predictions--", msg = msg)
      }

      # Drop NA rows (likely wrongly saved)
      post_dist_df_tier5 <- post_dist_df_tier5 %>% dplyr::filter(if_all(everything(), ~ !is.na(.)))
      post_dist_df_all <- post_dist_df_all %>% dplyr::filter(if_all(everything(), ~ !is.na(.)))

      # Stop if empty
      if (nrow(post_dist_df_tier5) == 0) {
        msg <- paste("No model outputs for the region")
        status:::status_log("ERROR", log_file = log_file, "--Model predictions--", msg = msg)
        stop("No model outputs found")
      }

      # ---- Process outputs for all tiers (data + new) ----
      for (tierIndex in seq(as.numeric(BY_TIER), 2)) {

        tier_col <- paste0("Tier", tierIndex)

        if (tier_col == "Tier5") {

          pred_tierIndex <- post_dist_df_tier5 %>%
            dplyr::rename(cover_prop = pred) %>%
            dplyr::select(fYEAR, !!sym(tier_col), draw, model_name, cover_prop)
        
        predictions <- reefCloudPackage::make_contrasts(pred_tierIndex, tier_col)

        pred_tierIndex <- dplyr::bind_rows(predictions) %>%
            dplyr::rename(
              Median = value, Lower = .lower, Upper = .upper,
              Fold.Change = fold_change, P.up = prob_up,
              P.down = prob_down, Change = arrow, Model.name = model_name,
              Year = year
            ) %>%
            dplyr::select(
              !!sym(tier_col), Year, Median, Lower, Upper, Fold.Change, P.up, P.down, Change, Model.name
            )


        } else if (tier_col == "Tier4") {

          sum_area <- reefCloudPackage::get_sum_area(post_dist_df_all, tier_col)

          pred_tierIndex <- post_dist_df_all %>%
            dplyr::group_by(fYEAR, draw, !!sym(tier_col), model_name) %>%
            dplyr::summarise(
              cover = sum(weighted_pred, na.rm = TRUE),
              .groups = "drop"
            ) %>%
            dplyr::left_join(sum_area, by = tier_col) %>%
            dplyr::mutate(
              cover_prop = cover / sum_area
            ) %>%
            dplyr::select(fYEAR, !!sym(tier_col), draw, model_name, cover_prop)

        predictions <- reefCloudPackage::make_contrasts(pred_tierIndex, tier_col)

        pred_tierIndex <- dplyr::bind_rows(predictions) %>%
            dplyr::rename(
              Median = value, Lower = .lower, Upper = .upper,
              Fold.Change = fold_change, P.up = prob_up,
              P.down = prob_down, Change = arrow, Model.name = model_name,
              Year = year
            ) %>%
            dplyr::select(
              !!sym(tier_col), Year, Median, Lower, Upper, Fold.Change, P.up, P.down, Change, Model.name
            )

        } else {

          sum_area <-  reefCloudPackage::get_sum_area(post_dist_df_all, tier_col)

          pred_tierIndex <- post_dist_df_all %>%
            dplyr::mutate(model_name = "FRK/INLA") %>%
            dplyr::group_by(fYEAR, draw, !!sym(tier_col), model_name) %>%
            dplyr::summarise(
              cover = sum(weighted_pred, na.rm = TRUE),
              .groups = "drop"
            ) %>%
            dplyr::left_join(sum_area, by = tier_col) %>%
            dplyr::mutate(
              cover_prop = cover / sum_area
            ) %>%
            dplyr::select(fYEAR, !!sym(tier_col), draw, model_name, cover_prop)

        predictions <- reefCloudPackage::make_contrasts(pred_tierIndex, tier_col)

        pred_tierIndex <- dplyr::bind_rows(predictions) %>%
            dplyr::rename(
              Median = value, Lower = .lower, Upper = .upper,
              Fold.Change = fold_change, P.up = prob_up,
              P.down = prob_down, Change = arrow, Model.name = model_name,
              Year = year
            ) %>%
            dplyr::select(
              !!sym(tier_col), Year, Median, Lower, Upper, Fold.Change, P.up, P.down, Change, Model.name
            )
        }
        
        # Extra information of the regions

         info_region <- extract_info_region(post_dist_df_all, tier_col)

        # ---- Save results into the AWS bucket ----
        readr::write_csv(
          pred_tierIndex,
          file = paste0(AWS_OUTPUT_PATH, "output_tier", tierIndex, ".csv"),
          quote = "none"
        )

             # ---- Save results into the AWS bucket ----
        readr::write_csv(
          info_region,
          file = paste0(AWS_OUTPUT_PATH, "info_tier", tierIndex, ".csv"),
          quote = "none"
        )
      }
      
      rm(pred_tierIndex, info_region)

      # ---- Process data-only tiers ----

      for (tierIndex in seq(as.numeric(BY_TIER), 2)) {

        tier_col <- paste0("Tier", tierIndex)

        if (tier_col == "Tier5") {

          pred_tierIndex <- post_dist_df_tier5 %>%
            filter(tier_type == "data") %>%
            dplyr::rename(cover_prop = pred) %>%
            dplyr::select(fYEAR, !!sym(tier_col), draw, model_name, cover_prop)

          predictions <- reefCloudPackage::make_contrasts(pred_tierIndex, tier_col)

          pred_tierIndex <- dplyr::bind_rows(predictions) %>%
            dplyr::rename(
              Median = value, Lower = .lower, Upper = .upper,
              Fold.Change = fold_change, P.up = prob_up,
              P.down = prob_down, Change = arrow, Model.name = model_name,
              Year = year
            ) %>%
            dplyr::select(
              !!sym(tier_col), Year, Median, Lower, Upper, Fold.Change, P.up, P.down, Change, Model.name
            )

        } else if (tier_col == "Tier4") {

          sum_area <- reefCloudPackage::get_sum_area(post_dist_df_all, tier_col, group = "data")

          pred_tierIndex <- post_dist_df_all %>%
            filter(tier_type == "data") %>%
            dplyr::group_by(fYEAR, draw, !!sym(tier_col), model_name) %>%
            dplyr::summarise(
              cover = sum(weighted_pred, na.rm = TRUE),
              .groups = "drop"
            ) %>%
            dplyr::left_join(sum_area, by = tier_col) %>%
            dplyr::mutate(
              cover_prop = cover / sum_area
            ) %>%
            dplyr::select(fYEAR, !!sym(tier_col), draw, model_name, cover_prop)

          predictions <- reefCloudPackage::make_contrasts(pred_tierIndex, tier_col)

          pred_tierIndex <- dplyr::bind_rows(predictions) %>%
            dplyr::rename(
              Median = value, Lower = .lower, Upper = .upper,
              Fold.Change = fold_change, P.up = prob_up,
              P.down = prob_down, Change = arrow, Model.name = model_name,
              Year = year
            ) %>%
            dplyr::select(
              !!sym(tier_col), Year, Median, Lower, Upper, Fold.Change, P.up, P.down, Change, Model.name
            )

        } else {

          sum_area <- reefCloudPackage::get_sum_area(post_dist_df_all, tier_col, group = "data")

          pred_tierIndex <- post_dist_df_all %>%
            filter(tier_type == "data") %>%
            dplyr::mutate(model_name = "FRK/INLA") %>%
            dplyr::group_by(fYEAR, draw, !!sym(tier_col), model_name) %>%
            dplyr::summarise(
              cover = sum(weighted_pred, na.rm = TRUE),
              .groups = "drop"
            ) %>%
            dplyr::left_join(sum_area, by = tier_col) %>%
            dplyr::mutate(
              cover_prop = cover / sum_area
            ) %>%
            dplyr::select(fYEAR, !!sym(tier_col), draw, model_name, cover_prop)

          predictions <- reefCloudPackage::make_contrasts(pred_tierIndex, tier_col)

          pred_tierIndex <- dplyr::bind_rows(predictions) %>%
            dplyr::rename(
              Median = value, Lower = .lower, Upper = .upper,
              Fold.Change = fold_change, P.up = prob_up,
              P.down = prob_down, Change = arrow, Model.name = model_name,
              Year = year
            ) %>%
            dplyr::select(
              !!sym(tier_col), Year, Median, Lower, Upper, Fold.Change, P.up, P.down, Change, Model.name
            )
        }

        # ---- Save data-tier results into the AWS bucket ----
        readr::write_csv(
          pred_tierIndex,
          file = paste0(AWS_OUTPUT_PATH, "output_tier", tierIndex, "_data.csv"),
          quote = "none"
        )
      }

    # ---- CASE 2: Other model types not implemented ----
    } else {
      msg <- paste("Code not updated for the model type", whichModel)
      status:::status_log("ERROR", log_file = log_file, "--Model outputs--", msg = msg)
      stop("Code not updated")
    }

  },
  stage_ = 4,
  order_ = 16,
  name_ = "Scaling-up model predictions and export",
  item_ = "model_prediction"
  )
}