#' @title Fit model at tier level
#' @description Fits FRK model to monitoring data at a given tier level, including covariate processing, quality control, model fitting and predictions.
#' @param data.grp.enough Data frame with sufficient data for modelling (filtered upstream)
#' @param tier.sf Tier-level shapefile (e.g., Tier3 or Tier4 spatial scale)
#' @examples
#' model_fitModelTier_type5(my_data_filtered, my_tier_sf)
#' @author Julie Vercelloni
#' @export
model_fitModelTier_type5_v2 <- function(data.grp.enough, tier.sf) {
  if (!exists("log_pipeline_step")) {
    log_pipeline_step <- function(...) invisible(NULL)
  }

  # Fetch GROUP from the global environment (set by model_fitModelTier via assign()).
  # Package namespace scoping prevents free-variable lookup from reaching
  # .GlobalEnv, so we resolve it explicitly here.
  GROUP <- get("GROUP", envir = .GlobalEnv)

  # Print version number
  version_file <- system.file("VERSION", package = "reefCloudPackage")
  if (file.exists(version_file)) {
    version <- readLines(version_file, n = 1)
    cat(paste0("\n### reefCloudPackage VERSION: ", version, "\n"))
  }

  FOCAL_TIER <- paste0("Tier", as.numeric(BY_TIER) - 1)
  data.grp <- data.grp.enough

  tiers <- unique(data.grp[[FOCAL_TIER]])

  # Single-tier diagnostic mode: filter to one focal-tier id when set.
  if (exists("SINGLE_TIER", envir = .GlobalEnv) && nzchar(SINGLE_TIER)) {
    tiers <- tiers[as.character(tiers) == as.character(SINGLE_TIER)]
    if (length(tiers) == 0) {
      cat(paste0(
        "ℹ Single-tier mode: ", FOCAL_TIER, " = ", SINGLE_TIER,
        " not present in this dataset; nothing to fit.\n"
      ))
      return(invisible(NULL))
    }
    cat(paste0(
      "ℹ Single-tier mode: limited to ", FOCAL_TIER, " = ",
      paste(tiers, collapse = ", "), "\n"
    ))
  }

  N <- length(tiers)

  #########################
  #### START LOOP BY TIER
  #########################
  for (i in seq_along(tiers)) {
    TIER <<- as.character(tiers[i])

    ## Filter to current tier

    log_pipeline_step(1, "Filter input data", list(
      "Total rows" = nrow(data.grp),
      "Focal tier" = FOCAL_TIER,
      "Current tier" = TIER
    ))

    data.grp.tier <- data.grp %>%
      dplyr::filter(data.grp[[FOCAL_TIER]] == TIER) %>%
      dplyr::select(-COVER) %>%
      dplyr::mutate(across(Tier5, as.character))

    cat(paste0("  Filtered rows: ", nrow(data.grp.tier), "\n"))
    cat(paste0("  Unique Tier5: ", length(unique(data.grp.tier$Tier5)), "\n"))
    cat(paste0("  Year range: ", min(data.grp.tier$REPORT_YEAR), " - ", max(data.grp.tier$REPORT_YEAR), "\n\n"))

    # Early temporal validation - FRK requires at least 3 time points
    n_years <- length(unique(data.grp.tier$REPORT_YEAR))
    if (n_years < 3) {
      msg <- paste0(
        "Insufficient temporal coverage for ", FOCAL_TIER, ": ", TIER, ". ",
        "Found ", n_years, " year(s), but FRK requires >=3 years for temporal modeling. ",
        "Skipping tier."
      )
      cat(paste0("  WARNING: ", msg, "\n\n"))
      status::status_try_catch(
        warning(msg),
        stage_ = 4,
        name_ = "Insufficient temporal data",
        item_ = "type5_temporal_check"
      )
      next
    }

    #--- Join covariates
    log_pipeline_step(2, "Join covariates to tier lookup", list())

    tier.sf.joined <- reefCloudPackage::join_covariates_to_tier_lookup(tier.sf, i, N) %>% #
      dplyr::filter(!!sym(FOCAL_TIER) == TIER)

    cat(paste0("  Joined tier rows: ", nrow(tier.sf.joined), "\n"))
    cat(paste0("  Columns: ", paste(names(tier.sf.joined), collapse = ", "), "\n"))
    cat(paste0("  Is sf object: ", inherits(tier.sf.joined, "sf"), "\n\n"))

    #--- Load and filter predictive layers
    log_pipeline_step(3, "Load and filter predictive layers", list())


    full_cov_raw <- reefCloudPackage::load_predictive_layers(i, N) %>%
      dplyr::filter(Tier5 %in% tier.sf.joined$Tier5) %>%
      dplyr::rename(fYEAR = year) %>%
      dplyr::filter(
        between(
          fYEAR,
          min(data.grp.tier$REPORT_YEAR),
          max(data.grp.tier$REPORT_YEAR)
        )
      )

    if (!inherits(full_cov_raw, "sf")) {
      message("full_cov_raw is not an sf object. Joining geometry from tier.sf.joined.")

      sf::sf_use_s2(FALSE)

      full_cov_raw <- full_cov_raw %>%
        dplyr::left_join(
          tier.sf.joined %>%
            dplyr::select(Tier5, geometry) %>%
            dplyr::distinct(Tier5, .keep_all = TRUE),
          by = "Tier5"
        ) %>%
        sf::st_as_sf(sf_column_name = "geometry")
    } else {
      message("full_cov_raw is already an sf object.")
    }

    # CRITICAL: Validate that loaded data is sf object
    cat("\n=== DEBUG: load_predictive_layers() output ===\n")
    cat("File loaded: ", files, "\n", sep = "")
    cat("Class: ", paste(class(full_cov_raw), collapse = ", "), "\n", sep = "")
    cat("Is sf: ", inherits(full_cov_raw, "sf"), "\n", sep = "")
    cat("Rows: ", nrow(full_cov_raw), "\n", sep = "")
    cat("Columns: ", ncol(full_cov_raw), "\n", sep = "")

    if (!inherits(full_cov_raw, "sf")) {
      warning(
        "CRITICAL: covariates_full_tier5.RData does not contain an sf object! ",
        "It contains: ", paste(class(full_cov_raw), collapse = ", ")
      )

      stop(
        "Predictive layers must be sf objects for spatial modeling. ",
        "Check data processing pipeline that creates covariates_full_tier5.RData"
      )
    } else {
      cat("Validation passed - full_cov_raw is an sf object\n\n")
    }
    cat(paste0("  Raw covariate rows: ", nrow(full_cov_raw), "\n"))
    cat(paste0("  Is sf object: ", inherits(full_cov_raw, "sf"), "\n"))
    cat(paste0("  Unique Tier5: ", length(unique(full_cov_raw$Tier5)), "\n"))
    cat(paste0("  Unique years: ", length(unique(full_cov_raw$fYEAR)), "\n\n"))


    #--- Apply control quality on extreme values
    log_pipeline_step(4, "Apply QC on extreme values", list())

    out_cycl <- quantile(full_cov_raw$max_cyc, probs = 0.975)
    out_dhw <- quantile(full_cov_raw$max_dhw, probs = 0.975)

    # Get geometry column to exclude from across() operations
    geom_col_qc <- attr(full_cov_raw, "sf_column")
    if (is.null(geom_col_qc)) geom_col_qc <- "geometry" # Default fallback

    HexPred_sf <- full_cov_raw %>%
      dplyr::mutate(As.Data = ifelse(Tier5 %in% data.grp.tier$Tier5, "Yes", "No")) %>%
      dplyr::mutate(across(
        matches("^max_cyc.*") & -any_of(geom_col_qc),
        ~ ifelse(.x >= out_cycl & As.Data == "No", NA, .x)
      )) %>%
      dplyr::mutate(across(
        matches("^max_dhw.*") & -any_of(geom_col_qc),
        ~ ifelse(.x >= out_dhw & As.Data == "No", NA, .x)
      ))

    log_pipeline_step(5, "HexPred_sf after QC (CRITICAL CHECKPOINT)", list(
      "Rows" = nrow(HexPred_sf),
      "Is sf object" = inherits(HexPred_sf, "sf"),
      "Has geometry" = !is.null(sf::st_geometry(HexPred_sf)),
      "Columns" = paste(names(HexPred_sf)[1:min(10, length(names(HexPred_sf)))], collapse = ", ")
    ))

    # Save HexPred_sf before any joins
    # save_debug_csv(
    #   sf::st_drop_geometry(HexPred_sf) %>% head(1000),
    #   debug_dir, "HexPred_sf_before_join", TIER, FOCAL_TIER, "05"
    # )

    #--- Select covariates
    log_pipeline_step(6, "Select covariates", list())

    selected_covar <- reefCloudPackage::select_covariates(HexPred_sf, i, N)
    selected_covar <- selected_covar[!grepl("\\.(lag3|lag5)$", selected_covar)] # only 2 lags used for now

    cat(paste0("  Selected covariates: ", paste(selected_covar, collapse = ", "), "\n"))
    cat(paste0("  Number of covariates: ", length(selected_covar), "\n\n"))

    ## Check for collinearity (threshold 0.7, keep lowest lag per group)
    log_pipeline_step(7, "Check for collinearity", list())

    selected_covar <- reefCloudPackage::filter_non_collinear(HexPred_sf, selected_covar, threshold = 0.7)
    cat(sprintf(
      "  Covariates after collinearity filter: %s\n",
      if (length(selected_covar) == 0) "(none)" else paste(selected_covar, collapse = ", ")
    ))

    # ---- Save tier checkpoint: Step 1 (covariates prepared) ----
    if (exists("SAVE_TIER_CHECKPOINTS", envir = .GlobalEnv) && SAVE_TIER_CHECKPOINTS) {
      checkpoint_name <- paste0("ch_", gsub("\\.", "_", GROUP), "_", FOCAL_TIER, "_", TIER)
      step1_data <- list(
        HexPred_sf = HexPred_sf,
        selected_covar = selected_covar,
        tier_sf_joined = tier.sf.joined,
        data_grp_tier_ready = data.grp.tier
      )
      reefCloudPackage::save_tier_checkpoint(checkpoint_name, TIER, 1, step1_data)
    }

    ## Add reefid and fill missing years
    log_pipeline_step(8, "Add reefid and fill missing years", list())

    covs.hexpred_tier_sf_v2_prep <- reefCloudPackage::make_reefid(tier.sf.joined, HexPred_sf, reef_layer.sf, i, N)

    HexPred_reefid <- covs.hexpred_tier_sf_v2_prep %>%
      dplyr::group_by(Tier5) %>%
      dplyr::summarise(reefid = paste0(reefid, collapse = "_")) %>%
      ungroup()

    HexPred_reefid2 <- dplyr::inner_join(
      sf::st_drop_geometry(HexPred_sf),
      HexPred_reefid,
      by = "Tier5"
    ) |>
      dplyr::group_by(Tier5, fYEAR) |>
      dplyr::filter(dplyr::row_number() == 1) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ replace(.x, is.na(.x), 0))) |>
      sf::st_as_sf(sf_column_name = "geometry")

    cat(paste0("  inner_join: ", nrow(HexPred_reefid2), " rows (reef-only BAUs)\n"))

    # Validate deduplication worked - check for any remaining duplicates
    log_pipeline_step(8, "Check duplicates in HexPred_reefid2", list())

    dup_check <- HexPred_reefid2 %>%
      sf::st_drop_geometry() %>%
      dplyr::group_by(Tier5, fYEAR) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      dplyr::filter(n > 1)

    if (nrow(dup_check) > 0) {
      msg <- paste0(
        "CRITICAL: Found ", nrow(dup_check), " duplicate (Tier5, fYEAR) combinations ",
        "in HexPred_reefid2 for ", FOCAL_TIER, ": ", TIER, ". ",
        "This will cause FRK dimension mismatch errors. Investigate data quality."
      )
      status::status_try_catch(
        warning(msg),
        stage_ = 4,
        name_ = "HexPred validation",
        item_ = "hexpred_validation"
      )
    }

    #--- Filter observations outside Tier5
    log_pipeline_step(9, "Filter observations outside Tier5", list(
      "Observations before" = nrow(data.grp.tier)
    ))

    data.grp.tier.ready <- reefCloudPackage::rm_obs_outside(data.grp.tier, HexPred_reefid2, i, N)

    cat("rm_obs_outside completed successfully\n")
    cat(paste0("Rows after filtering: ", nrow(data.grp.tier.ready), "\n\n"))

    ## Skip if more than 30% of observations are outside Tier5 cells (Julie's rule)
    diff_perc <- ((nrow(data.grp.tier) - nrow(data.grp.tier.ready)) / nrow(data.grp.tier)) * 100

    if (diff_perc > 30) {
      msg <- paste0(round(diff_perc, 1), "% of data locations are outside Tier5 cells for ", FOCAL_TIER, ": ", TIER, " — skipping.")
      cat(paste0("  WARNING: ", msg, "\n"))
      message(msg)

      # Track failed tier in TIER_STATUS
      if (exists("TIER_STATUS", envir = .GlobalEnv)) {
        TIER_STATUS[[TIER]] <<- list(
          status = "failed_all",
          method = "none",
          n_sites = length(unique(data.grp.tier$Tier5)),
          n_years = length(unique(data.grp.tier$fYEAR)),
          failure_reason = paste0(round(diff_perc, 1), "% of observations outside Tier5 cells")
        )
      }
      next
    }

    #--- Final spatiotemporal coverage validation (after all filtering)
    log_pipeline_step(10, "Validate spatiotemporal coverage after filtering", list(
      "Observations after all filtering" = nrow(data.grp.tier.ready)
    ))

    # Check temporal coverage
    n_years_final <- length(unique(data.grp.tier.ready$REPORT_YEAR))
    n_sites_final <- length(unique(data.grp.tier.ready$Tier5))

    cat(paste0("  Unique years after filtering: ", n_years_final, "\n"))
    cat(paste0("  Unique sites (Tier5) after filtering: ", n_sites_final, "\n"))

    if (n_years_final < 2) {
      msg <- paste0(
        "Insufficient temporal coverage after filtering for ", FOCAL_TIER, ": ", TIER, ". ",
        "Found ", n_years_final, " year(s) after removing observations outside tier boundaries. ",
        "FRK requires at least 2 time points for temporal modeling. ",
        "Skipping tier."
      )
      cat(paste0("  ERROR: ", msg, "\n\n"))
      status::status_try_catch(
        warning(msg),
        stage_ = 4,
        name_ = "Insufficient temporal coverage after filtering",
        item_ = "type5_temporal_check_post_filter"
      )
      next
    }

    if (n_sites_final < 3) {
      msg <- paste0(
        "Insufficient spatial coverage after filtering for ", FOCAL_TIER, ": ", TIER, ". ",
        "Found ", n_sites_final, " site(s) (Tier5) after removing observations outside tier boundaries. ",
        "FRK requires at least 3 spatial locations for spatial modelling. ",
        "Skipping tier."
      )
      cat(paste0("  ERROR: ", msg, "\n\n"))
      status::status_try_catch(
        warning(msg),
        stage_ = 4,
        name_ = "Insufficient spatial coverage after filtering",
        item_ = "type5_spatial_check_post_filter"
      )
      next
    }

    cat(paste0("  ✓ Spatiotemporal coverage check passed: ", n_years_final, " years × ", n_sites_final, " sites\n\n"))

    ## Test if more than one reef in the final data
    test_reefid <- HexPred_reefid2 %>% filter(Tier5 %in% data.grp.tier.ready$Tier5)

    log_pipeline_step(11, "Validate reefid", list(
      "Prediction rows matched to retained Tier5" = nrow(test_reefid),
      "Unique reef IDs retained" = length(unique(test_reefid$reefid)),
      "Multiple reef IDs present" = ifelse(length(unique(test_reefid$reefid)) > 1, "Yes", "No")
    ))


    #--- Prepare model objects
    log_pipeline_step(12, "Call frk_prep_v2() to prepare FRK model inputs", list(
      "Data rows" = nrow(data.grp.tier.ready),
      "HexPred_reefid2 rows" = nrow(HexPred_reefid2),
      "HexPred_reefid2 is sf" = inherits(HexPred_reefid2, "sf")
    ))

    # Get basis resolution from environment variable, default to 2L
    basis_res <- if (exists("BASIS_RESOLUTION")) as.integer(BASIS_RESOLUTION) else 3L
    obj_frk <- reefCloudPackage::frk_prep_v2(data.grp.tier.ready, HexPred_reefid2, i, N, basis_resolution = basis_res)

    # ---- Save tier checkpoint: Step 2 (FRK inputs prepared) ----
    if (exists("SAVE_TIER_CHECKPOINTS", envir = .GlobalEnv) && SAVE_TIER_CHECKPOINTS && !is.null(obj_frk)) {
      checkpoint_name <- paste0("ch_", gsub("\\.", "_", GROUP), "_", FOCAL_TIER, "_", TIER)
      step2_data <- list(
        obj_frk = obj_frk,
        data_grp_tier_ready = data.grp.tier.ready,
        HexPred_reefid2 = HexPred_reefid2,
        selected_covar = selected_covar
      )
      reefCloudPackage::save_tier_checkpoint(checkpoint_name, TIER, 2, step2_data)
    }

    # CRITICAL: Check if frk_prep() succeeded
    if (is.null(obj_frk)) {
      msg <- paste0(
        "FRK Preparation Failed: Could not create spatiotemporal grid for ",
        FOCAL_TIER, ": ", TIER, ". ",
        "Possible causes: invalid spatial/temporal structure, insufficient data coverage. ",
        "Review 'Prep FRK objects' logs above for details. Skipping tier."
      )
      status::status_try_catch(
        warning(msg),
        stage_ = 4,
        name_ = "FRK prep validation",
        item_ = "validate_frk_prep"
      )

      # Track failed tier in TIER_STATUS
      if (exists("TIER_STATUS", envir = .GlobalEnv)) {
        TIER_STATUS[[TIER]] <<- list(
          status = "failed_all",
          method = "none",
          n_sites = length(unique(data.grp.tier.ready$Tier5)),
          n_years = length(unique(data.grp.tier.ready$fYEAR)),
          failure_reason = "FRK preparation failed - invalid spatiotemporal structure"
        )
      }
      next
    }

    # Validate components of obj_frk
    if (is.null(obj_frk$ST_BAUs)) {
      msg <- paste0(
        "Invalid BAUs Grid: Spatiotemporal grid (ST_BAUs) is NULL for ",
        FOCAL_TIER, ": ", TIER, ". Cannot proceed with model fitting. Skipping tier."
      )
      status::status_try_catch(warning(msg), stage_ = 4, name_ = "BAUs validation", item_ = "validate_baus")

      if (exists("TIER_STATUS", envir = .GlobalEnv)) {
        TIER_STATUS[[TIER]] <<- list(
          status = "failed_all",
          method = "none",
          n_sites = length(unique(data.grp.tier.ready$Tier5)),
          n_years = length(unique(data.grp.tier.ready$fYEAR)),
          failure_reason = "Invalid BAUs grid (ST_BAUs is NULL)"
        )
      }
      next
    }
    if (is.null(obj_frk$STObj)) {
      msg <- paste0(
        "Invalid Observation Object: Spatiotemporal observations (STObj) NULL for ",
        FOCAL_TIER, ": ", TIER, ". Cannot proceed with model fitting. Skipping tier."
      )
      status::status_try_catch(warning(msg), stage_ = 4, name_ = "STObj validation", item_ = "validate_stobj")

      if (exists("TIER_STATUS", envir = .GlobalEnv)) {
        TIER_STATUS[[TIER]] <<- list(
          status = "failed_all",
          method = "none",
          n_sites = length(unique(data.grp.tier.ready$Tier5)),
          n_years = length(unique(data.grp.tier.ready$fYEAR)),
          failure_reason = "Invalid observation object (STObj is NULL)"
        )
      }
      next
    }
    if (is.null(obj_frk$basis)) {
      msg <- paste0(
        "Invalid Basis Functions: Spatial basis is NULL for ",
        FOCAL_TIER, ": ", TIER, ". Cannot proceed with model fitting. Skipping tier."
      )
      status::status_try_catch(warning(msg), stage_ = 4, name_ = "Basis validation", item_ = "validate_basis")

      if (exists("TIER_STATUS", envir = .GlobalEnv)) {
        TIER_STATUS[[TIER]] <<- list(
          status = "failed_all",
          method = "none",
          n_sites = length(unique(data.grp.tier.ready$Tier5)),
          n_years = length(unique(data.grp.tier.ready$fYEAR)),
          failure_reason = "Invalid basis functions (basis is NULL)"
        )
      }
      next
    }

    ## === DATA VALIDATION ===

    # Validate sufficient data for modeling
    n_obs <- nrow(data.grp.tier.ready)
    n_locations <- length(unique(data.grp.tier.ready$Tier5))
    n_years <- length(unique(data.grp.tier.ready$fYEAR))
    n_covars <- length(selected_covar)

    # Check 1: Covariate variance and collinearity
    if (length(selected_covar) > 0) {
      # Extract covariate matrix
      cov_matrix <- obj_frk$ST_BAUs@data[, selected_covar, drop = FALSE]
      cov_matrix_complete <- cov_matrix[complete.cases(cov_matrix), ]

      # Check sufficient data coverage (at least 50% complete cases)
      coverage_pct <- (nrow(cov_matrix_complete) / nrow(cov_matrix)) * 100
      if (coverage_pct < 50) {
        msg <- paste0(
          "Covariate Quality: Only ", round(coverage_pct, 1), "% of BAUs have complete covariate data for ",
          FOCAL_TIER, ": ", TIER, " [", nrow(cov_matrix_complete), "/", nrow(cov_matrix), " BAUs]. ",
          "Model predictions will rely on spatial smoothing in data-sparse areas."
        )
        status::status_try_catch(
          message(msg),
          stage_ = 4,
          name_ = "Covariate coverage",
          item_ = "covariate_coverage"
        )
      }

      # Skip variance/correlation checks if insufficient data
      if (nrow(cov_matrix_complete) < 10) {
        msg <- paste0(
          "Covariate Preprocessing: Too few complete cases (n=", nrow(cov_matrix_complete), ") for ",
          FOCAL_TIER, ": ", TIER, ". Skipping covariate quality checks - model may be unstable."
        )
        status::status_try_catch(
          warning(msg),
          stage_ = 4,
          name_ = "Insufficient covariate data",
          item_ = "insufficient_cov_data"
        )
      } else {
        # Check variance
        zero_var_covs <- names(cov_matrix_complete)[apply(cov_matrix_complete, 2, sd, na.rm = TRUE) < 1e-10]
        if (length(zero_var_covs) > 0) {
          msg <- paste0(
            "Covariate Preprocessing: Removing ", length(zero_var_covs), " zero-variance covariate(s) for ",
            FOCAL_TIER, ": ", TIER, " [", paste(zero_var_covs, collapse = ", "), "]. ",
            "These provide no predictive information."
          )
          status::status_try_catch(
            message(msg),
            stage_ = 4,
            name_ = "Remove uninformative covariates",
            item_ = "remove_zero_variance"
          )
          # Remove zero-variance covariates
          selected_covar <- setdiff(selected_covar, zero_var_covs)

          # Check if ALL covariates were removed
          if (length(selected_covar) == 0) {
            msg <- paste0(
              "Model Structure: All covariates removed for ", FOCAL_TIER, ": ", TIER, ". ",
              "Fitting intercept-only model (spatial-temporal patterns only, no environmental predictors)."
            )
            status::status_try_catch(
              message(msg),
              stage_ = 4,
              name_ = "Intercept-only model",
              item_ = "intercept_only_model"
            )
          }
        }
      } # End else block for sufficient data check
    }

    # Check 2: Sufficient degrees of freedom
    n_params <- 1 + length(unique(test_reefid$reefid)) + length(selected_covar)
    dof_ratio <- n_obs / n_params
    if (dof_ratio < 3) {
      msg <- paste0(
        "Model Complexity Warning: Degrees of freedom ratio is ", round(dof_ratio, 1), ":1 for ",
        FOCAL_TIER, ": ", TIER, " [", n_obs, " obs / ", n_params, " parameters]. ",
        "Recommended minimum 3:1. Model may be overparameterized - ",
        "predictions may have high uncertainty or fitting may fail."
      )
      status::status_try_catch(
        message(msg),
        stage_ = 4,
        name_ = "Degrees of freedom check",
        item_ = "check_dof_ratio"
      )
    }

    ## === END VALIDATION ===

    #--- Build formula
    log_pipeline_step(13, "Build model formula", list(
      "Number of covariates" = length(selected_covar),
      "Number of unique reefids" = length(unique(test_reefid$reefid))
    ))

    ## Define model formula
    model_formula <- if (length(selected_covar) == 0 && length(unique(test_reefid$reefid)) > 1) {
      as.formula("COUNT ~ 1 + (1 | reefid)")
    } else if (length(selected_covar) == 0 && length(unique(test_reefid$reefid)) == 1) {
      as.formula("COUNT ~ 1")
    } else if (length(selected_covar) != 0 && length(unique(test_reefid$reefid)) == 1) {
      as.formula(paste("COUNT ~ 1 +", paste(selected_covar, collapse = " + ")))
    } else {
      as.formula(paste("COUNT ~ 1 + (1 | reefid) +", paste(selected_covar, collapse = " + ")))
    }

    ## Fit FRK model
    log_pipeline_step(14, "Fit FRK model", list(
      "Response" = "binomial",
      "Link" = "logit"
    ))

    M <- tryCatch(
      {
        # Track which formula is actually used (may change in fallback)
        actual_formula <- model_formula
        fitting_method <- "TMB"

        # Attempt 1: Full model with TMB method
        M <- tryCatch(
          {
            FRK::FRK(
              f = model_formula,
              data = list(obj_frk$STObj),
              BAUs = obj_frk$ST_BAUs,
              basis = obj_frk$basis,
              response = "binomial",
              link = "logit",
              K_type = "precision",
              method = "TMB",
              est_error = FALSE
            )
          },
          error = function(e) {
            # Check if it's a singular matrix or numerical optimization error
            if (grepl("singular|U\\[.*\\].*=.*0|NA/NaN gradient|not positive definite", e$message, ignore.case = TRUE)) {
              msg <- paste0(
                "Model Fitting: TMB method failed with singular matrix error for ", FOCAL_TIER, ": ", TIER, ". ",
                "This usually indicates perfect collinearity or numerical instability. ",
                "Attempting fallback strategies..."
              )
              status::status_try_catch(
                message(msg),
                stage_ = 4,
                name_ = "TMB method failed",
                item_ = "tmb_singular_matrix"
              )

              # Fallback 1: Try with EM method instead of TMB
              M_fallback <- tryCatch(
                {
                  msg_fallback1 <- paste0(
                    "Model Fitting: Attempting EM algorithm (more robust, slower) for ",
                    FOCAL_TIER, ": ", TIER
                  )
                  status::status_try_catch(
                    message(msg_fallback1),
                    stage_ = 4,
                    name_ = "Fallback 1: EM algorithm",
                    item_ = "fallback_em_method"
                  )

                  fitting_method <<- "EM" # Update method tracking

                  FRK::FRK(
                    f = model_formula,
                    data = list(obj_frk$STObj),
                    BAUs = obj_frk$ST_BAUs,
                    basis = obj_frk$basis,
                    response = "binomial",
                    link = "logit",
                    K_type = "precision",
                    method = "EM",
                    est_error = FALSE
                  )
                },
                error = function(e2) {
                  # Fallback 2: Try simpler model (remove covariates)
                  if (length(selected_covar) > 0) {
                    msg <- paste0(
                      "Model Fitting: EM algorithm also failed for ", FOCAL_TIER, ": ", TIER, ". ",
                      "Attempting intercept-only model (spatial-temporal baseline without covariates)..."
                    )
                    status::status_try_catch(
                      message(msg),
                      stage_ = 4,
                      name_ = "Fallback 2: Intercept-only",
                      item_ = "fallback_intercept_only"
                    )

                    # Simplest formula: intercept only
                    simple_formula <- if (length(unique(test_reefid$reefid)) > 1) {
                      as.formula("COUNT ~ 1 + (1 | reefid)")
                    } else {
                      as.formula("COUNT ~ 1")
                    }

                    actual_formula <<- simple_formula # Update formula tracking
                    fitting_method <<- "TMB-intercept" # Update method tracking

                    tryCatch(
                      {
                        FRK::FRK(
                          f = simple_formula,
                          data = list(obj_frk$STObj),
                          BAUs = obj_frk$ST_BAUs,
                          basis = obj_frk$basis,
                          response = "binomial",
                          link = "logit",
                          K_type = "precision",
                          method = "TMB",
                          est_error = FALSE
                        )
                      },
                      error = function(e3) {
                        # All fallbacks failed
                        msg <- paste0(
                          "Model Fitting: All strategies exhausted for ", FOCAL_TIER, ": ", TIER, ". ",
                          "TMB, EM, and intercept-only models all failed. ",
                          "Data may be incompatible with FRK framework. Skipping tier."
                        )
                        status::status_try_catch(
                          stop(msg),
                          stage_ = 4,
                          name_ = "All fitting strategies failed",
                          item_ = "all_methods_failed"
                        )
                        return(NULL)
                      }
                    )
                  } else {
                    # No covariates to remove, can't simplify further
                    msg <- paste0(
                      "Model Fitting: TMB and EM methods failed for ", FOCAL_TIER, ": ", TIER, ". ",
                      "Already using intercept-only model - cannot simplify further. ",
                      "Data structure may be incompatible with FRK. Skipping tier."
                    )
                    status::status_try_catch(
                      stop(msg),
                      stage_ = 4,
                      name_ = "Cannot simplify model",
                      item_ = "cannot_simplify_model"
                    )
                    return(NULL)
                  }
                }
              )

              return(M_fallback)
            } else {
              # Not a singular matrix error, re-throw
              stop(e$message)
            }
          }
        )

        # Update status (wrapped in tryCatch to prevent crash)
        tryCatch(
          {
            old_item_name <- get_status_name(4, "FRK_fit")
            if (!str_detect(old_item_name, "\\[")) {
              new_item_name <- paste(old_item_name, "[", i, " / ", N, "]")
            } else {
              new_item_name <- str_replace(old_item_name, "\\[([^\\]]*)\\]", paste("[", i, " / ", N, "]"))
            }
          },
          error = function(e) NULL
        )

        M # Return M from the tryCatch expression
      },
      error = function(e) {
        cat(sprintf(
          "  [WARN] FRK model fitting failed for %s: %s: %s\n",
          FOCAL_TIER, TIER, conditionMessage(e)
        ))
        NULL
      }
    )

    ## Handle failed model (accept both FRK and SRE classes)
    if (is.null(M) || (!inherits(M, "FRK") && !inherits(M, "SRE"))) {
      model_type <- if (is.null(M)) "NULL" else class(M)[1]
      msg <- paste0(
        "Model Validation: Model fitting returned invalid object for ", FOCAL_TIER, ": ", TIER, ". ",
        "Expected FRK or SRE model object, got ", model_type, ". ",
        "Review fitting errors above. Skipping tier."
      )
      status::status_try_catch(
        warning(msg),
        stage_ = 4,
        name_ = "Invalid model object",
        item_ = "invalid_model_object"
      )

      # Track failed tier in TIER_STATUS
      if (exists("TIER_STATUS", envir = .GlobalEnv)) {
        TIER_STATUS[[TIER]] <<- list(
          status = "failed_all",
          method = "none",
          n_sites = length(unique(data.grp.tier.ready$Tier5)),
          n_years = length(unique(data.grp.tier.ready$fYEAR)),
          failure_reason = "Model fitting returned invalid object"
        )
      }
      next
    }

    # ---- Save tier checkpoint: Step 3 (model fitted) ----
    if (exists("SAVE_TIER_CHECKPOINTS", envir = .GlobalEnv) && SAVE_TIER_CHECKPOINTS) {
      checkpoint_name <- paste0("ch_", gsub("\\.", "_", GROUP), "_", FOCAL_TIER, "_", TIER)
      step3_data <- list(
        M = M,
        actual_formula = actual_formula,
        fitting_method = fitting_method,
        obj_frk = obj_frk,
        selected_covar = selected_covar,
        data_grp_tier_ready = data.grp.tier.ready
      )
      reefCloudPackage::save_tier_checkpoint(checkpoint_name, TIER, 3, step3_data)
    }

    #   ##############################
    #   #### Predict & summarise
    #   ##############################

    log_pipeline_step(15, "Generate predictions (500 simulations)", list())

    pred <- tryCatch(
      {
        FRK::predict(M, type = "mean", nsim = 500)
      },
      error = function(e) {
        # SRE models may need explicit newdata parameter
        if (inherits(M, "SRE")) {
          msg <- paste0(
            "Prediction Error: SRE predict() failed for ", FOCAL_TIER, ": ", TIER, ". ",
            "Error: ", e$message, ". ",
            "SRE models from EM method may have prediction incompatibilities. Skipping tier."
          )
          status::status_try_catch(
            warning(msg),
            stage_ = 4,
            name_ = "SRE prediction failed",
            item_ = "sre_prediction_error"
          )
          return(NULL)
        } else {
          # Re-throw for FRK models
          stop(e$message)
        }
      }
    )

    # Validate prediction output
    if (is.null(pred)) {
      msg <- paste0(
        "Prediction Failed: predict() returned NULL for ", FOCAL_TIER, ": ", TIER, ". ",
        "Model may not have converged properly. Skipping tier."
      )
      status::status_try_catch(warning(msg), stage_ = 4, name_ = "Prediction is NULL", item_ = "pred_is_null")

      if (exists("TIER_STATUS", envir = .GlobalEnv)) {
        TIER_STATUS[[TIER]] <<- list(
          status = "failed_all",
          method = fitting_method,
          n_sites = length(unique(data.grp.tier.ready$Tier5)),
          n_years = length(unique(data.grp.tier.ready$fYEAR)),
          failure_reason = "Prediction returned NULL - model may not have converged"
        )
      }
      next
    }
    if (is.null(pred$MC)) {
      msg <- paste0(
        "Prediction Failed: Monte Carlo samples missing for ", FOCAL_TIER, ": ", TIER, ". ",
        "Prediction structure invalid. Skipping tier."
      )
      status::status_try_catch(warning(msg), stage_ = 4, name_ = "MC samples missing", item_ = "mc_samples_missing")

      if (exists("TIER_STATUS", envir = .GlobalEnv)) {
        TIER_STATUS[[TIER]] <<- list(
          status = "failed_all",
          method = fitting_method,
          n_sites = length(unique(data.grp.tier.ready$Tier5)),
          n_years = length(unique(data.grp.tier.ready$fYEAR)),
          failure_reason = "Monte Carlo samples missing from prediction"
        )
      }
      next
    }
    if (is.null(pred$MC$mu_samples)) {
      msg <- paste0(
        "Prediction Failed: Mean samples (mu_samples) missing for ", FOCAL_TIER, ": ", TIER, ". ",
        "Cannot compute posterior distributions. Skipping tier."
      )
      status::status_try_catch(warning(msg), stage_ = 4, name_ = "mu_samples missing", item_ = "mu_samples_missing")

      if (exists("TIER_STATUS", envir = .GlobalEnv)) {
        TIER_STATUS[[TIER]] <<- list(
          status = "failed_all",
          method = fitting_method,
          n_sites = length(unique(data.grp.tier.ready$Tier5)),
          n_years = length(unique(data.grp.tier.ready$fYEAR)),
          failure_reason = "Mean samples (mu_samples) missing from prediction"
        )
      }
      next
    }

    # Check dimension compatibility before mutate
    n_pred_rows <- nrow(pred$MC$mu_samples)
    n_bau_rows <- nrow(obj_frk$ST_BAUs@data)
    if (n_pred_rows != n_bau_rows) {
      msg <- paste0(
        "Prediction Failed: Dimension mismatch for ", FOCAL_TIER, ": ", TIER, ". ",
        "Predictions: ", n_pred_rows, " rows, BAUs: ", n_bau_rows, " rows. ",
        "BAU grid and predictions incompatible - likely BAU temporal filtering issue. Skipping tier."
      )
      status::status_try_catch(warning(msg), stage_ = 4, name_ = "Dimension mismatch", item_ = "dimension_mismatch")

      if (exists("TIER_STATUS", envir = .GlobalEnv)) {
        TIER_STATUS[[TIER]] <<- list(
          status = "failed_all",
          method = fitting_method,
          n_sites = length(unique(data.grp.tier.ready$Tier5)),
          n_years = length(unique(data.grp.tier.ready$fYEAR)),
          failure_reason = paste0("Dimension mismatch: predictions (", n_pred_rows, ") vs BAUs (", n_bau_rows, ")")
        )
      }
      next
    }

    post_dist_df <- as.data.frame(pred$MC$mu_samples) %>%
      dplyr::mutate(
        fYEAR = obj_frk$ST_BAUs@data$fYEAR,
        Tier5 = obj_frk$ST_BAUs@data$Tier5,
        id_loc = row_number()
      ) %>%
      tidyr::pivot_longer(!c(fYEAR, Tier5, id_loc), names_to = "draw", values_to = "pred") %>%
      dplyr::mutate(model_name = "FRK")

    # Ensure type consistency for join - convert both to factor with same levels
    tier.sf.joined$Tier5 <- as.factor(tier.sf.joined$Tier5)
    post_dist_df$Tier5 <- factor(post_dist_df$Tier5, levels = levels(tier.sf.joined$Tier5))

    # Extract geometry separately (drop geometry to avoid sf join issues)
    tier_geom <- tier.sf.joined %>%
      dplyr::select(Tier5, geometry) %>%
      dplyr::distinct(Tier5, .keep_all = TRUE)

    pred_sum_sf <- post_dist_df %>%
      group_by(fYEAR, Tier5) %>%
      ggdist::median_hdci(pred) %>%
      dplyr::left_join(sf::st_drop_geometry(tier_geom), by = "Tier5") %>%
      dplyr::left_join(tier_geom %>% dplyr::select(Tier5, geometry), by = "Tier5") %>%
      sf::st_as_sf() %>%
      dplyr::mutate(
        Unc = .upper - .lower,
        Tier5_fYEAR = paste0(Tier5, fYEAR)
      )

    #   ##############################
    #   #### Save outputs
    #   ##############################
    # NB: this used to be wrapped in status::status_try_catch, which silently
    # swallowed every error here (it was hitting an internal `if (debug_mode)`
    # check with a NULL `debug_mode`, breaking error handling) and as a result
    # no tier ever produced an .RData file in /data/modelled. We now use a
    # plain tryCatch and surface the error message via cat() so future
    # failures are visible in the run log.
    save_err <- tryCatch(
      {
        group_slug <- gsub("[ /]", "_", GROUP)
        model_file <- paste0(DATA_PATH, "modelled/", "FRK_", group_slug, "_", FOCAL_TIER, "_", TIER, ".RData")

        log_pipeline_step(16, "Save model outputs", list(
          "Output file" = basename(model_file)
        ))

        saveRDS(
          list(
            group = GROUP,
            form = actual_formula, # Save the formula that was actually used
            fitting_method = fitting_method, # Track which method succeeded
            pred_sum_sf = pred_sum_sf,
            post_dist_df = post_dist_df,
            data.grp.tier = data.grp.tier,
            M = M
          ),
          file = model_file
        )

        cat(paste0("  ✓ Model outputs saved successfully\n\n"))
        cat(paste0("### COMPLETED TIER ", i, " of ", N, ": ", FOCAL_TIER, " = ", TIER, "\n"))
        cat(paste0(strrep("#", 80), "\n\n"))

        # Sync this tier's model to S3 immediately (intermediate checkpoint)
        data_path_env <- Sys.getenv("ORIGINAL_DATA_PATH", unset = "")
        if (data_path_env != "" && grepl("^s3://", data_path_env)) {
          s3_dest <- paste0(data_path_env, "/modelled/")
          sync_cmd <- sprintf(
            "aws s3 cp '%s' '%s' --region ap-southeast-2 --only-show-errors 2>&1",
            model_file,
            paste0(s3_dest, basename(model_file))
          )
          sync_result <- suppressWarnings(system(sync_cmd, intern = TRUE, ignore.stderr = FALSE))
          if (length(sync_result) > 0 && any(grepl("error|Error|ERROR", sync_result))) {
            message("S3 sync warning for ", FOCAL_TIER, ": ", TIER, " - ", paste(sync_result, collapse = " "))
          } else {
            message(
              "Checkpoint: Synced ", basename(model_file), " to S3 (",
              round(file.size(model_file) / 1024 / 1024, 1), " MB)"
            )
          }
        }

        # ---- Save tier checkpoint: Step 4 (predictions generated) ----
        if (exists("SAVE_TIER_CHECKPOINTS", envir = .GlobalEnv) && SAVE_TIER_CHECKPOINTS) {
          checkpoint_name <- paste0("ch_", gsub("\\.", "_", GROUP), "_", FOCAL_TIER, "_", TIER)
          step4_data <- list(
            pred_sum_sf = pred_sum_sf,
            post_dist_df = post_dist_df,
            model_file = model_file,
            final_output_saved = TRUE
          )
          reefCloudPackage::save_tier_checkpoint(checkpoint_name, TIER, 4, step4_data)
        }

        # Track successful tier in TIER_STATUS
        if (exists("TIER_STATUS", envir = .GlobalEnv)) {
          TIER_STATUS[[TIER]] <<- list(
            status = "complete",
            method = fitting_method, # "TMB", "EM", or "TMB-intercept"
            n_sites = length(unique(data.grp.tier.ready$Tier5)),
            n_years = length(unique(data.grp.tier.ready$fYEAR)),
            n_obs = nrow(data.grp.tier.ready),
            formula = deparse(actual_formula),
            model_file = basename(model_file)
          )
        }

        # Update status (guard against missing/empty status name)
        tryCatch(
          {
            old_item_name <- get_status_name(4, "FRK_saved")
            if (length(old_item_name) > 0 && nzchar(old_item_name)) {
              if (!str_detect(old_item_name, "\\[")) {
                new_item_name <- paste(old_item_name, "[", i, " / ", N, "]")
              } else {
                new_item_name <- str_replace(old_item_name, "\\[([^\\]]*)\\]", paste("[", i, " / ", N, "]"))
              }
            }
          },
          error = function(e) NULL
        ) # Ignore status display errors

        NULL # tryCatch body return
      },
      error = function(e) e
    )
    if (inherits(save_err, "error")) {
      msg <- paste0(
        "Save block failed for ", FOCAL_TIER, " = ", TIER,
        ": ", save_err$message
      )
      cat("  [WARN] type5: ", msg, "\n", sep = "")
    }

    # ---- Cleanup: free large objects before next tier iteration ----
    rm_vars <- c(
      "M", "pred", "post_dist_df", "pred_sum_sf", "obj_frk",
      "HexPred_sf", "HexPred_reefid2", "full_cov_raw",
      "data.grp.tier", "data.grp.tier.ready", "tier.sf.joined",
      "selected_covar", "test_reefid", "step1_data", "step2_data",
      "step3_data", "step4_data"
    )
    for (v in rm_vars) {
      if (exists(v, inherits = FALSE)) rm(list = v)
    }
    gc()
  }
}
