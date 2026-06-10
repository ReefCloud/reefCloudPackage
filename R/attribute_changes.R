#' @title Attribute Changes by Model Type
#' @description Compiles coefficient estimates from FRK and INLA models by Tier level,
#' extracting uncertainty measures and formatting results into a common structure.
#'
#' @return A CSV file is written to AWS_OUTPUT_PATH and a tibble of combined estimates is returned (invisibly).
#' @examples
#' attribute_changes("Tier4")
#' @author Julie Vercelloni
#' @export
attribute_changes <- function() {

  cat("\n")
  cat(strrep("-", 70), "\n")
  cat("attribute_changes: Starting\n")
  cat(strrep("-", 70), "\n")

  # ---- Verify required globals ----
  required_globals <- c("DATA_PATH", "AWS_OUTPUT_PATH", "BY_TIER")
  missing_globals <- required_globals[!sapply(required_globals, exists, envir = .GlobalEnv)]
  if (length(missing_globals) > 0) {
    stop(sprintf("attribute_changes: Missing required globals: %s", paste(missing_globals, collapse = ", ")))
  }

  # ---- Load input data tables for modelling ----
  # NOTE: load_data_for_model() still uses status::status_try_catch internally.
  # If it fails silently, tiers.lookup might not be refreshed — but it's already
  # in .GlobalEnv from the parent's scale_up_pred call moments earlier.
  cat("  Loading data for model...\n")
  reefCloudPackage::load_data_for_model()

  # Validate critical globals are available
  if (!exists("tiers.lookup", envir = .GlobalEnv)) {
    stop("attribute_changes: tiers.lookup not found in .GlobalEnv after load_data_for_model(). Data may be missing.")
  }

  FOCAL_TIER <- paste0('Tier', as.numeric(BY_TIER) - 1)
  cat(sprintf("  Focal tier: %s\n", FOCAL_TIER))

  # ---- Find model output files ----
  files <- list.files(
    path = paste0(DATA_PATH, "modelled"),
    pattern = "FRK|INLA",
    full.names = TRUE
  )
  files <- files[!grepl('TIER', files, perl = TRUE)]

  if (length(files) == 0) {
    stop(sprintf("attribute_changes: No model output files (FRK|INLA) found in %s/modelled/", DATA_PATH))
  }

  cat(sprintf("  Found %d model files\n", length(files)))

  # ---- First pass: extract group labels ----
  # NOTE: This loads each full RDS just for obj$group. The group IS parseable
  # from the filename (FRK_HARD_CORAL_Tier4_1808.RData) but we keep this for
  # robustness with any legacy file naming.
  group_list <- character(length(files))
  for (i in seq_along(files)) {
    obj <- tryCatch(
      readRDS(files[i]),
      error = function(e) {
        stop(sprintf("attribute_changes: Failed to read %s: %s", basename(files[i]), e$message))
      }
    )
    group_list[i] <- if (!is.null(obj$group)) obj$group else NA_character_
    rm(obj); gc()
  }

  # Warn about missing group labels
  n_na_groups <- sum(is.na(group_list))
  if (n_na_groups > 0) {
    cat(sprintf("  WARNING: %d files have NULL group label (will be excluded from coef_table)\n", n_na_groups))
  }

  # ---- Extract model name and Tier ID from filenames ----
  # New filename format includes group slug: FRK_HARD_CORAL_Tier4_1819.RData
  info <- stringr::str_match(basename(files), "^(FRK|INLA)_.+_Tier\\d+_(\\d+)\\.RData$")
  # Fallback for legacy filenames without group slug: FRK_Tier4_1819.RData
  legacy <- is.na(info[, 1])
  if (any(legacy)) {
    info_legacy <- stringr::str_match(basename(files[legacy]), "^(FRK|INLA)_Tier\\d+_(\\d+)\\.RData$")
    info[legacy, ] <- info_legacy
  }

  # Validate filename parsing
  unparsed <- is.na(info[, 1])
  if (any(unparsed)) {
    bad_files <- basename(files[unparsed])
    stop(sprintf("attribute_changes: Could not parse model/tier from filename(s): %s",
                 paste(bad_files, collapse = ", ")))
  }

  dist_df <- tibble::tibble(
    file = files,
    model_name = info[, 2],
    Group = group_list,
    !!sym(FOCAL_TIER) := as.integer(info[, 3])
  )

  # ---- Prepare coefficient table lists ----
  coef_table_list_FRK <- list()
  coef_table_list_INLA <- list()

  # Find the position of FOCAL_TIER column in tiers.lookup
  tiers.lookup <- tiers.lookup %>%
    dplyr::select(tier_id, reef_area, Tier5, Tier4, Tier3, Tier2)
  start_col <- which(colnames(tiers.lookup) == FOCAL_TIER)

  if (length(start_col) == 0) {
    stop(sprintf("attribute_changes: FOCAL_TIER '%s' not found in tiers.lookup columns: %s",
                 FOCAL_TIER, paste(colnames(tiers.lookup), collapse = ", ")))
  }

  # ---- Second pass: extract M and compute coefficients (memory-efficient) ----
  # Load one model at a time, compute coef_uncertainty, free immediately.
  # Peak memory: ~3-5 GB (one M object + computation overhead).
  cat(sprintf("  Processing %d model files for coefficient uncertainty...\n", length(files)))
  n_errors <- 0

  for (i in seq_along(files)) {
    cat(sprintf("    [%d/%d] %s (%s)...",
                i, length(files), basename(files[i]), dist_df$model_name[i]))

    # Load the model object
    obj <- tryCatch(
      readRDS(files[i]),
      error = function(e) {
        cat(sprintf(" ERROR reading file: %s\n", e$message))
        n_errors <<- n_errors + 1
        NULL
      }
    )
    if (is.null(obj)) next

    M <- obj$M
    rm(obj); gc()

    if (is.null(M)) {
      cat(" WARNING: M is NULL, skipping\n")
      n_errors <- n_errors + 1
      next
    }

    # ---- FRK model coefficients ----
    if (dist_df$model_name[i] == "FRK") {
      coef_result <- tryCatch({
        FRK::coef_uncertainty(
          M, percentiles = c(2.5, 50, 97.5), nsim = 400, random_effects = FALSE
        ) %>%
          data.frame() %>%
          tibble::rownames_to_column() %>%
          tidyr::pivot_longer(cols = !rowname, names_to = "term", values_to = "value") %>%
          tidyr::pivot_wider(names_from = rowname, values_from = value) %>%
          dplyr::mutate(
            Group = dist_df$Group[i],
            model_name = dist_df$model_name[i],
            !!sym(FOCAL_TIER) := as.factor(dist_df[[FOCAL_TIER]][i])
          ) %>%
          dplyr::left_join(tiers.lookup %>%
            dplyr::select(all_of(colnames(tiers.lookup)[start_col:ncol(tiers.lookup)])) %>%
            dplyr::distinct())
      }, error = function(e) {
        cat(sprintf(" ERROR in coef_uncertainty: %s\n", e$message))
        n_errors <<- n_errors + 1
        NULL
      })

      if (!is.null(coef_result)) {
        coef_table_list_FRK[[i]] <- coef_result
        cat(" OK\n")
      }

    # ---- INLA model coefficients ----
    } else if (dist_df$model_name[i] == "INLA") {
      coef_result <- tryCatch({
        # INLA $summary.fixed gives coefficients on the logit (linear predictor) scale.
        # Reporting on the logit scale matches what FRK::coef_uncertainty returns,
        # so the coef_table is unit-consistent across models.
        M$summary.fixed %>%
          tibble::rownames_to_column("term") %>%
          dplyr::select(term, `0.025quant`, mean, `0.975quant`) %>%
          dplyr::mutate(
            `50%` = mean,
            `2.5%` = `0.025quant`,
            `97.5%` = `0.975quant`
          ) %>%
          dplyr::select(term, `2.5%`, `50%`, `97.5%`) %>%
          dplyr::mutate(
            Group = dist_df$Group[i],
            model_name = dist_df$model_name[i],
            !!sym(FOCAL_TIER) := as.factor(dist_df[[FOCAL_TIER]][i]),
            term = gsub("[()]", "", term)
          ) %>%
          dplyr::left_join(tiers.lookup %>%
            dplyr::select(all_of(colnames(tiers.lookup)[start_col:ncol(tiers.lookup)])) %>%
            dplyr::distinct())
      }, error = function(e) {
        cat(sprintf(" ERROR extracting INLA coefficients: %s\n", e$message))
        n_errors <<- n_errors + 1
        NULL
      })

      if (!is.null(coef_result)) {
        coef_table_list_INLA[[i]] <- coef_result
        cat(" OK\n")
      }

    } else {
      cat(sprintf(" ERROR: Unsupported model type '%s'\n", dist_df$model_name[i]))
      n_errors <- n_errors + 1
    }

    # Free M immediately — only one in memory at a time
    rm(M); gc()
  }

  # ---- Validate: stop if too many errors ----
  total_files <- length(files)
  success_count <- length(coef_table_list_FRK) + length(coef_table_list_INLA) -
    sum(vapply(coef_table_list_FRK, is.null, logical(1))) - sum(vapply(coef_table_list_INLA, is.null, logical(1)))

  if (success_count == 0) {
    stop(sprintf("attribute_changes: ALL %d model files failed coefficient extraction. Cannot produce coef_table.",
                 total_files))
  }
  if (n_errors > 0) {
    cat(sprintf("\n  WARNING: %d of %d files had errors during coefficient extraction\n", n_errors, total_files))
    # Allow up to 25% failures (some tiers may have convergence issues)
    if (n_errors > total_files * 0.25) {
      stop(sprintf("attribute_changes: Too many errors (%d/%d). Aborting to prevent invalid output.",
                   n_errors, total_files))
    }
  }

  # ---- Combine coefficient tables ----
  coef_table <- dplyr::bind_rows(coef_table_list_FRK, coef_table_list_INLA)

  if (nrow(coef_table) == 0) {
    stop("attribute_changes: coef_table has 0 rows after binding. No valid coefficients extracted.")
  }

  # Add the type of model if covariates or not — per Group + focal tier
  coef_table <- coef_table %>%
    dplyr::group_by(Group, .data[[FOCAL_TIER]]) %>%
    dplyr::mutate(
      model_type = dplyr::case_when(
        all(term == "Intercept") ~ "Intercept only",
        all(stringr::str_detect(term, "^Intercept$|^fYEAR")) ~ "Intercept and year effects only",
        TRUE ~ "Includes disturbance effects"
      )
    ) %>%
    dplyr::ungroup()

  # Log NA warning (non-fatal)
  if (anyNA(coef_table)) {
    na_cols <- names(coef_table)[sapply(coef_table, anyNA)]
    cat(sprintf("  WARNING: NAs in coef_table columns: %s. Removing NA rows.\n",
                paste(na_cols, collapse = ", ")))
  }

  # Remove NAs and validate
  coef_table <- coef_table %>%
    dplyr::filter(if_all(everything(), ~ !is.na(.)))

  if (nrow(coef_table) == 0) {
    stop("attribute_changes: coef_table has 0 rows after removing NAs. All Group labels may be NULL.")
  }

  # ---- Rename terms for human readability ----
  coef_table <- coef_table %>%
    dplyr::mutate(term = dplyr::case_when(
      stringr::str_detect(term, "^fYEAR\\d{4}$") ~ stringr::str_replace(term, "^fYEAR(\\d{4})$", "Year \\1"),
      term == "Intercept"      ~ "Intercept",
      term == "max_cyc"        ~ "Cyclone exposure",
      term == "max_cyc_lag1"   ~ "Cyclone exposure (lag1)",
      term == "max_cyc_lag2"   ~ "Cyclone exposure (lag2)",
      term == "max_dhw"        ~ "Heat stress",
      term == "max_dhw_lag1"   ~ "Heat stress (lag1)",
      term == "max_dhw_lag2"   ~ "Heat stress (lag2)",
      TRUE ~ term
    )) %>%
    dplyr::rename(
      Variable = term, Median = `50%`, Lower = `2.5%`,
      Upper = `97.5%`, Model.name = model_name, Model.type = model_type
    ) %>%
    dplyr::select(
      Group,
      starts_with(FOCAL_TIER),
      Variable, Median, Lower, Upper, Model.name, Model.type,
      everything()
    )

  # ---- Write output ----
  output_file <- paste0(AWS_OUTPUT_PATH, "coef_table.csv")
  tryCatch(
    readr::write_csv(coef_table, file = output_file, quote = "none"),
    error = function(e) {
      stop(sprintf("attribute_changes: Failed to write %s: %s", output_file, e$message))
    }
  )

  cat("\n")
  cat(strrep("-", 70), "\n")
  cat(sprintf("attribute_changes: COMPLETE. Wrote %s (%d rows)\n", basename(output_file), nrow(coef_table)))
  cat(sprintf("  Models processed: %d FRK + %d INLA = %d total\n",
              sum(!vapply(coef_table_list_FRK, is.null, logical(1))),
              sum(!vapply(coef_table_list_INLA, is.null, logical(1))),
              success_count))
  if (n_errors > 0) {
    cat(sprintf("  Errors: %d (non-fatal, within tolerance)\n", n_errors))
  }
  cat(sprintf("  Memory: %.0f MB\n", sum(gc()[, 2])))
  cat(strrep("-", 70), "\n")

  invisible(coef_table)
}
