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

  cat("\n")
  cat(strrep("-", 70), "\n")
  cat("scale_up_pred: Starting (model type = ", whichModel, ")\n", sep = "")
  cat(strrep("-", 70), "\n")

  # ---- Validate inputs ----
  if (!whichModel %in% c("type5", "type6")) {
    stop(sprintf("scale_up_pred: Unsupported model type '%s'. Only type5/type6 implemented.", whichModel))
  }

  # ---- Verify required globals ----
  required_globals <- c("DATA_PATH", "AWS_OUTPUT_PATH", "BY_TIER")
  missing_globals <- required_globals[!sapply(required_globals, exists, envir = .GlobalEnv)]
  if (length(missing_globals) > 0) {
    stop(sprintf("scale_up_pred: Missing required globals: %s", paste(missing_globals, collapse = ", ")))
  }

  # ---- Find model output files ----
  modelled_dir <- paste0(DATA_PATH, "modelled")
  if (!dir.exists(modelled_dir)) {
    stop(sprintf("scale_up_pred: Model directory does not exist: %s", modelled_dir))
  }

  files <- list.files(path = modelled_dir, pattern = "FRK|INLA", full.names = TRUE)
  files <- files[!grepl('TIER', files, perl = TRUE)]

  if (length(files) == 0) {
    stop(sprintf("scale_up_pred: No model output files (FRK|INLA) found in %s", modelled_dir))
  }

  cat(sprintf("  Found %d model output files\n", length(files)))

  # ---- Load tier lookup (small) ----
  tiers_lookup_path <- file.path(DATA_PATH, "primary", "tiers.lookup.RData")
  if (!file.exists(tiers_lookup_path)) {
    stop(sprintf("scale_up_pred: tiers.lookup.RData not found at %s", tiers_lookup_path))
  }
  load(tiers_lookup_path, envir = .GlobalEnv)

  # ---- Process files ONE AT A TIME to avoid OOM ----
  # Strategy: for each file, load post_dist_df, join with tiers.lookup,
  # compute aggregates at each tier level, free raw data immediately.
  # This keeps peak memory to ~1 file's post_dist_df at a time.

  # Accumulators for per-file aggregated results (small)
  tier5_contrasts_data <- list()
  tier4_agg_data <- list()      # per-draw sums at Tier4 level (data-only Tier5s)
  all_data_tier5s <- character(0)  # track all Tier5s that had data (for sum_area computation)

  for (i in seq_along(files)) {
    cat(sprintf("  Processing %d/%d: %s\n", i, length(files), basename(files[i])))

    tier <- stringr::str_extract(files[i], "(?<=_)(\\d+)(?=.RData)")

    obj <- tryCatch(
      readRDS(files[i]),
      error = function(e) {
        stop(sprintf("scale_up_pred: Failed to read model file %s: %s", basename(files[i]), e$message))
      }
    )

    # Temporary renaming for compatibility
    if ("data.sub" %in% names(obj)) {
      names(obj)[names(obj) == "data.sub"] <- "data.grp.tier"
    }

    # Group label
    group_label <- if (!is.null(obj$group)) {
      obj$group
    } else {
      parsed <- stringr::str_match(basename(files[i]), "^(?:FRK|INLA)_(.+)_Tier\\d+_\\d+\\.RData$")[, 2]
      if (is.na(parsed)) NA_character_ else gsub("_", " ", parsed)
    }

    # Validate critical fields
    if (is.null(obj$post_dist_df)) {
      cat(sprintf("    WARNING: %s has NULL post_dist_df. Skipping.\n", basename(files[i])))
      rm(obj); gc()
      next
    }
    if (is.null(obj$data.grp.tier)) {
      cat(sprintf("    WARNING: %s has NULL data.grp.tier. Skipping.\n", basename(files[i])))
      rm(obj); gc()
      next
    }

    pdf <- obj$post_dist_df
    pdf$Group <- group_label

    # Determine which Tier5s had actual data
    data_tier5s <- unique(obj$data.grp.tier$Tier5)
    all_data_tier5s <- union(all_data_tier5s, as.character(data_tier5s))

    # Free the large model object immediately
    rm(obj); gc()

    # Add tier_type
    pdf <- pdf |>
      dplyr::mutate(
        tier_type = ifelse(as.character(Tier5) %in% data_tier5s, "data", "new")
      )

    # Validate model_name column exists
    if (!"model_name" %in% names(pdf)) {
      cat(sprintf("    WARNING: %s missing 'model_name' column. Skipping.\n", basename(files[i])))
      rm(pdf, data_tier5s); gc()
      next
    }

    # Standardize column types
    pdf <- pdf |>
      dplyr::mutate(
        Group = as.character(Group),
        fYEAR = as.factor(fYEAR),
        Tier5 = as.factor(Tier5),
        draw = as.character(draw),
        pred = as.numeric(pred),
        model_name = as.character(model_name),
        tier_type = as.character(tier_type)
      )

    # Join with tiers.lookup to get higher tier columns and reef_area
    pdf <- pdf |>
      dplyr::left_join(tiers.lookup, by = "Tier5") |>
      dplyr::mutate(
        reef_area = reef_area / 1000000,
        weighted_pred = pred * reef_area
      )

    # ---- Tier5 contrasts (computed per-file, data-only Tier5s) ----
    tier5_data <- tryCatch({
      pdf_data <- pdf |> dplyr::filter(tier_type == "data")
      if (nrow(pdf_data) > 0) {
        tmp <- pdf_data |>
          dplyr::rename(cover_prop = pred) |>
          dplyr::select(Group, fYEAR, Tier5, draw, model_name, cover_prop)
        dplyr::bind_rows(reefCloudPackage::make_contrasts(tmp, "Tier5"))
      } else {
        NULL
      }
    }, error = function(e) {
      cat(sprintf("    WARNING: Tier5 data-only contrasts failed for %s: %s\n", basename(files[i]), e$message))
      NULL
    })
    if (!is.null(tier5_data)) {
      tier5_contrasts_data[[length(tier5_contrasts_data) + 1]] <- tier5_data
    }

    # ---- Aggregate to Tier4+ level (data-only Tier5s) ----
    # Each file is one Tier4, so after aggregation we get ~(ndraws x nyears) rows = ~20K rows
    agg_data <- pdf |>
      dplyr::filter(tier_type == "data") |>
      dplyr::group_by(Group, fYEAR, draw, Tier4, Tier3, Tier2, model_name) |>
      dplyr::summarise(cover = sum(weighted_pred, na.rm = TRUE), .groups = "drop")
    if (nrow(agg_data) > 0) {
      tier4_agg_data[[length(tier4_agg_data) + 1]] <- agg_data
    }

    # Free the large per-file data
    rm(pdf, agg_data, data_tier5s); gc()

    cat(sprintf("    Done. Memory: %.0f MB\n", sum(gc()[, 2])))
  }

  cat(sprintf("\n  All files processed. Memory: %.0f MB\n", sum(gc()[, 2])))

  # ---- Combine aggregated results (these are small) ----
  cat("  Combining aggregated results...\n")
  combined_agg_data <- dplyr::bind_rows(tier4_agg_data)
  rm(tier4_agg_data); gc()

  cat(sprintf("  Combined Tier4 aggregation: %s rows (data-only)\n",
              format(nrow(combined_agg_data), big.mark = ",")))

  # ---- Process outputs for all tier levels (DATA-ONLY aggregation) ----
  # Primary outputs use data-only Tier5 aggregation (matching Julie's approach):
  # - Only Tier5s with actual observations are included
  # - sum_area computed only from data Tier5s
  # This produces wider, more honest CIs that reflect actual data coverage.
  cat("\n  Computing predictions and contrasts (data-only aggregation)...\n")
  output_files_written <- character(0)

  for (tierIndex in seq(as.numeric(BY_TIER), 2)) {

    tier_col <- paste0("Tier", tierIndex)
    cat(sprintf("    Tier %d...\n", tierIndex))

    pred_tierIndex <- tryCatch({
      if (tier_col == "Tier5") {

        # Tier5: use data-only contrasts (computed per-file)
        result <- dplyr::bind_rows(tier5_contrasts_data)
        if (nrow(result) == 0) {
          result
        } else {
          result |>
            dplyr::rename(
              Median = value, Lower = .lower, Upper = .upper,
              Fold.Change = fold_change, P.up = prob_up,
              P.down = prob_down, Change = arrow, Model.name = model_name,
              Year = year
            ) |>
            dplyr::select(
              Group, !!sym(tier_col), Year, Median, Lower, Upper, Fold.Change, P.up, P.down, Change, Model.name
            )
        }

      } else {

        # For Tier4/3/2: aggregate from combined_agg_data (data-only Tier5s)
        # Compute sum_area from tiers.lookup filtered to data-only Tier5s
        sum_area <- tiers.lookup |>
          dplyr::filter(as.character(Tier5) %in% all_data_tier5s) |>
          dplyr::mutate(reef_area = reef_area / 1000000) |>
          dplyr::group_by(!!sym(tier_col)) |>
          dplyr::summarise(sum_area = sum(reef_area, na.rm = TRUE), .groups = "drop")

        tmp <- combined_agg_data |>
          dplyr::group_by(Group, fYEAR, draw, !!sym(tier_col), model_name) |>
          dplyr::summarise(cover = sum(cover, na.rm = TRUE), .groups = "drop") |>
          dplyr::left_join(sum_area, by = tier_col) |>
          dplyr::mutate(cover_prop = cover / sum_area) |>
          dplyr::select(Group, fYEAR, !!sym(tier_col), draw, model_name, cover_prop)

        # For Tier2/3 use generic model_name
        if (!tier_col %in% c("Tier5", "Tier4")) {
          tmp <- tmp |> dplyr::mutate(model_name = "FRK/INLA")
        }

        predictions <- reefCloudPackage::make_contrasts(tmp, tier_col)

        dplyr::bind_rows(predictions) |>
          dplyr::rename(
            Median = value, Lower = .lower, Upper = .upper,
            Fold.Change = fold_change, P.up = prob_up,
            P.down = prob_down, Change = arrow, Model.name = model_name,
            Year = year
          ) |>
          dplyr::select(
            Group, !!sym(tier_col), Year, Median, Lower, Upper, Fold.Change, P.up, P.down, Change, Model.name
          )
      }
    }, error = function(e) {
      stop(sprintf("scale_up_pred: Failed computing contrasts for %s: %s", tier_col, e$message))
    })

    # Validate output before writing
    if (nrow(pred_tierIndex) == 0) {
      stop(sprintf("scale_up_pred: Contrasts for %s produced 0 rows. Model outputs may be invalid.", tier_col))
    }

    # Compute regional info from tiers.lookup + all_data_tier5s
    info_region <- tryCatch({
      # Size.area: total reef area per tier unit (ALL Tier5s in region, not just data)
      # This is a geographic descriptor of the region's total reef extent.
      # Matches Julie's get_sum_area.R which sums over all Tier5s without filtering.
      sum_area_info <- tiers.lookup |>
        dplyr::mutate(reef_area_km2 = reef_area / 1000000) |>
        dplyr::group_by(Tier5) |>
        dplyr::slice_head(n = 1) |>
        dplyr::ungroup() |>
        dplyr::group_by(!!sym(tier_col)) |>
        dplyr::summarise(Size.area = sum(reef_area_km2, na.rm = TRUE), .groups = "drop")

      # Year.range: from combined_agg_data
      year_range_info <- combined_agg_data |>
        dplyr::mutate(yr = as.numeric(as.character(fYEAR))) |>
        dplyr::group_by(Group, !!sym(tier_col)) |>
        dplyr::summarise(
          Year.range = paste0(min(yr, na.rm = TRUE), "\u2013", max(yr, na.rm = TRUE)),
          .groups = "drop"
        )

      # data.tier / new.tier: % of Tier5 sub-units with observations per tier
      tier5_coverage <- tiers.lookup |>
        dplyr::select(Tier5, !!sym(tier_col)) |>
        dplyr::distinct() |>
        dplyr::mutate(has_data = as.character(Tier5) %in% all_data_tier5s) |>
        dplyr::group_by(!!sym(tier_col)) |>
        dplyr::summarise(
          data.tier = sum(has_data) / dplyr::n() * 100,
          new.tier = sum(!has_data) / dplyr::n() * 100,
          .groups = "drop"
        )

      # FRK.prop / INLA.prop: from combined_agg_data model_name counts per tier
      model_prop_info <- combined_agg_data |>
        dplyr::group_by(Group, !!sym(tier_col)) |>
        dplyr::count(model_name) |>
        dplyr::mutate(prop = (n / sum(n)) * 100) |>
        dplyr::select(Group, !!sym(tier_col), model_name, prop) |>
        tidyr::pivot_wider(names_from = model_name, values_from = prop, values_fill = 0) |>
        dplyr::ungroup()
      if (!"FRK" %in% names(model_prop_info)) model_prop_info$FRK <- 0
      if (!"INLA" %in% names(model_prop_info)) model_prop_info$INLA <- 0
      model_prop_info <- model_prop_info |>
        dplyr::rename(FRK.prop = FRK, INLA.prop = INLA)

      # Join all pieces
      all_info <- year_range_info |>
        dplyr::left_join(sum_area_info, by = tier_col) |>
        dplyr::left_join(tier5_coverage, by = tier_col) |>
        dplyr::left_join(model_prop_info, by = c("Group", tier_col)) |>
        dplyr::select(Group, !!sym(tier_col), Size.area, Year.range,
                      data.tier, new.tier, FRK.prop, INLA.prop)

      all_info
    }, error = function(e) {
      cat(sprintf("    WARNING: info_region computation failed for %s: %s (continuing)\n", tier_col, e$message))
      NULL
    })

    # ---- Write output CSV ----
    output_file <- paste0(AWS_OUTPUT_PATH, "output_tier", tierIndex, ".csv")
    tryCatch(
      readr::write_csv(pred_tierIndex, file = output_file, quote = "none"),
      error = function(e) {
        stop(sprintf("scale_up_pred: Failed to write %s: %s", output_file, e$message))
      }
    )
    output_files_written <- c(output_files_written, output_file)
    cat(sprintf("      -> wrote %s (%s rows)\n", basename(output_file), format(nrow(pred_tierIndex), big.mark = ",")))

    # ---- Write info CSV ----
    if (!is.null(info_region)) {
      info_file <- paste0(AWS_OUTPUT_PATH, "info_tier", tierIndex, ".csv")
      tryCatch(
        readr::write_csv(info_region, file = info_file, quote = "none"),
        error = function(e) {
          cat(sprintf("    WARNING: Failed to write %s: %s (non-fatal)\n", info_file, e$message))
        }
      )
      output_files_written <- c(output_files_written, info_file)
      cat(sprintf("      -> wrote %s\n", basename(info_file)))
    }
  }

  # ---- Final validation ----
  if (length(output_files_written) == 0) {
    stop("scale_up_pred: No output files were written. Something went wrong in the prediction pipeline.")
  }

  cat("\n")
  cat(strrep("-", 70), "\n")
  cat(sprintf("scale_up_pred: COMPLETE. Wrote %d files to %s\n", length(output_files_written), AWS_OUTPUT_PATH))
  cat(sprintf("  Memory: %.0f MB\n", sum(gc()[, 2])))
  cat(strrep("-", 70), "\n")

  invisible(output_files_written)
}
