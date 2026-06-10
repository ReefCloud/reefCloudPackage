#' @title Fit model at tier level
#' @description Fits FRK model to monitoring data at a given tier level, including covariate processing, quality control, model fitting and predictions.
#' @param data.grp.enough Data frame with sufficient data for modelling (filtered upstream)
#' @param tier.sf Tier-level shapefile (e.g., Tier3 or Tier4 spatial scale)
#' @examples
#' model_fitModelTier_type5(my_data_filtered, my_tier_sf)
#' @author Julie Vercelloni
#' @export
model_fitModelTier_type5 <- function(data.grp.enough, tier.sf){
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

  FOCAL_TIER <- paste0('Tier', as.numeric(BY_TIER) - 1)
  data.grp <- data.grp.enough

  tiers <- unique(data.grp[[FOCAL_TIER]])

  # Single-tier diagnostic mode: filter to one focal-tier id when set.
  if (exists("SINGLE_TIER", envir = .GlobalEnv) && nzchar(SINGLE_TIER)) {
    tiers <- tiers[as.character(tiers) == as.character(SINGLE_TIER)]
    if (length(tiers) == 0) {
      cat(paste0("ℹ Single-tier mode: ", FOCAL_TIER, " = ", SINGLE_TIER,
                 " not present in this dataset; nothing to fit.\n"))
      return(invisible(NULL))
    }
    cat(paste0("ℹ Single-tier mode: limited to ", FOCAL_TIER, " = ",
               paste(tiers, collapse = ", "), "\n"))
  }

  N <- length(tiers)

  #########################
  #### START LOOP BY TIER
  #########################
  for (i in seq_along(tiers)) {
    TIER <<- as.character(tiers[i])

    ## Filter to current tier
    data.grp.tier <- data.grp %>%
      dplyr::filter(data.grp[[FOCAL_TIER]] == TIER) %>%
      dplyr::select(-COVER) %>%
      dplyr::mutate(across(Tier5, as.character))

    ## Join covariates
    tier.sf.joined <- reefCloudPackage::join_covariates_to_tier_lookup(tier.sf, i, N) %>% 
      dplyr::filter(!!sym(FOCAL_TIER) == TIER)

    ## Load covariate layers
    full_cov_raw <- reefCloudPackage::load_predictive_layers(i , N) %>% 
      dplyr::filter(Tier5 %in% tier.sf.joined$Tier5) %>%
      dplyr::rename(fYEAR = year) %>%
      dplyr::filter(between(fYEAR, min(data.grp.tier$REPORT_YEAR), max(data.grp.tier$REPORT_YEAR)))

    ## Apply quality control thresholds
    out_cycl <- quantile(full_cov_raw$max_cyc, probs = 0.975)
    out_dhw  <- quantile(full_cov_raw$max_dhw, probs = 0.975)

    # Get geometry column to exclude from across() operations
    geom_col_qc <- attr(full_cov_raw, "sf_column")
    if (is.null(geom_col_qc)) geom_col_qc <- "geometry"  # Default fallback

    HexPred_sf <- full_cov_raw %>%
      dplyr::mutate(As.Data = ifelse(Tier5 %in% data.grp.tier$Tier5, "Yes", "No")) %>%
      dplyr::mutate(across(matches("^max_cyc.*") & -all_of(geom_col_qc), ~ ifelse(.x >= out_cycl & As.Data == "No", NA, .x))) %>%
      dplyr::mutate(across(matches("^max_dhw.*") & -all_of(geom_col_qc), ~ ifelse(.x >= out_dhw & As.Data == "No", NA, .x)))

    ## Select covariates
    selected_covar <- reefCloudPackage::select_covariates(HexPred_sf, i , N)

    ## Check for collinearity (threshold 0.7, keep lowest lag per group)
    selected_covar <- reefCloudPackage::filter_non_collinear(HexPred_sf, selected_covar, threshold = 0.7)
    cat(sprintf("  Covariates after collinearity filter: %s\n",
                if (length(selected_covar) == 0) "(none)" else paste(selected_covar, collapse = ", ")))

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

    ## Scale covariates
   # HexPred_sf <- HexPred_sf %>%
   #   dplyr::mutate(across(
   #    matches("^severity.*|^max.*"),
   #   ~ as.numeric((. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE))
   #  ))

    ## Add reefid and fill missing years
    covs.hexpred_tier_sf_v2_prep <- reefCloudPackage::make_reefid(tier.sf.joined, HexPred_sf, reef_layer.sf, i , N) 

   HexPred_reefid <- covs.hexpred_tier_sf_v2_prep %>%
      dplyr::group_by(Tier5) %>%
      dplyr::summarise(reefid = paste0(reefid, collapse = "_")) %>%
      ungroup()

    # Use inner_join to keep only Tier5s that have a reefid (i.e., intersect with reef layer).
    # This matches Julie's approach: only reef-containing hexagons become BAUs for FRK.
    # With left_join, all ~1110 Tier5s were kept as BAUs but only ~50 had data,
    # causing 67% NA covariates (filled with 0), over-smoothing, and wrong covariate signs.
    HexPred_reefid2 <- dplyr::inner_join(
        HexPred_sf |> data.frame(),
        sf::st_drop_geometry(HexPred_reefid),
        by = "Tier5"
      ) |>
      dplyr::group_by(Tier5, fYEAR) |>
      dplyr::filter(dplyr::row_number() == 1) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ replace(.x, is.na(.x), 0))) |>
      sf::st_as_sf(sf_column_name = "geometry")

    cat(paste0("  inner_join: ", nrow(HexPred_reefid2), " rows (reef-only BAUs)\n"))

    # Validate deduplication worked - check for any remaining duplicates
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

    ## Remove obs outside covariate grid
    data.grp.tier.ready <- reefCloudPackage::rm_obs_outside(data.grp.tier, HexPred_reefid2, i , N)
    cat("rm_obs_outside completed successfully\n")
    cat(paste0("Filtered rows: ", nrow(data.grp.tier.ready), "\n\n"))    

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

    ## Test if more than one reef in the final data 
    test_reefid <- HexPred_reefid2 %>% filter(Tier5 %in% data.grp.tier.ready$Tier5)

    ## Prep FRK model inputs
    # Get basis resolution from environment variable, default to 2L
    basis_res <- if (exists("BASIS_RESOLUTION")) as.integer(BASIS_RESOLUTION) else 3L
    obj_frk <- reefCloudPackage::frk_prep(data.grp.tier.ready, HexPred_reefid2, i, N, basis_resolution = basis_res)

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

    # Check 1: Minimum observations
    if (n_obs < 30) {
      msg <- paste0(
        "Data Validation: Insufficient observations (n=", n_obs, ") for ",
        FOCAL_TIER, ": ", TIER, ". ",
        "Spatial-temporal models require minimum 30 observations for reliable parameter estimation. ",
        "Skipping tier."
      )
      status::status_try_catch(
        warning(msg),
        stage_ = 4,
        name_ = "Data quantity check",
        item_ = "validate_data_quantity"
      )

      if (exists("TIER_STATUS", envir = .GlobalEnv)) {
        TIER_STATUS[[TIER]] <<- list(
          status = "failed_all",
          method = "none",
          n_sites = n_locations,
          n_years = n_years,
          n_obs = n_obs,
          failure_reason = paste0("Insufficient observations (n=", n_obs, ", minimum 30 required)")
        )
      }
      next
    }

    # Check 2: Observations per year (temporal coverage)
    obs_per_year <- table(data.grp.tier.ready$fYEAR)
    if (any(obs_per_year < 3)) {
      sparse_years <- names(obs_per_year)[obs_per_year < 3]
      n_sparse <- length(sparse_years)
      msg <- paste0(
        "Temporal Coverage Warning: ", n_sparse, " year(s) with <3 observations for ",
        FOCAL_TIER, ": ", TIER, " [", paste(sparse_years, collapse=", "), "]. ",
        "Temporal patterns for these years will have high uncertainty. ",
        "Total coverage: ", n_years, " years with ", n_obs, " observations."
      )
      status::status_try_catch(
        message(msg),
        stage_ = 4,
        name_ = "Temporal coverage check",
        item_ = "check_temporal_coverage"
      )
    }

    # Check 2b: Minimum observations per location (spatial coverage)
    obs_per_location <- n_obs / n_locations
    if (obs_per_location < 2) {
      msg <- paste0(
        "Spatial Coverage Warning: Low replication (", round(obs_per_location, 1), " obs/location) for ",
        FOCAL_TIER, ": ", TIER, " [", n_obs, " obs across ", n_locations, " locations]. ",
        "Spatial patterns may be poorly estimated. Model will rely heavily on basis functions."
      )
      status::status_try_catch(
        message(msg),
        stage_ = 4,
        name_ = "Spatial coverage check",
        item_ = "check_spatial_coverage"
      )
    }

    # Check 3: Covariate variance and collinearity
    if (length(selected_covar) > 0) {
      # Extract covariate matrix
      cov_matrix <- obj_frk$ST_BAUs@data[, selected_covar, drop=FALSE]
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
      zero_var_covs <- names(cov_matrix_complete)[apply(cov_matrix_complete, 2, sd, na.rm=TRUE) < 1e-10]
      if (length(zero_var_covs) > 0) {
        msg <- paste0(
          "Covariate Preprocessing: Removing ", length(zero_var_covs), " zero-variance covariate(s) for ",
          FOCAL_TIER, ": ", TIER, " [", paste(zero_var_covs, collapse=", "), "]. ",
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

      }  # End else block for sufficient data check
    }

    # Check 4: Sufficient degrees of freedom
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

    ## Define model formula
    model_formula <- if (length(selected_covar) == 0 && length(unique(test_reefid$reefid)) > 1) {
        as.formula("COUNT ~ 1 + (1 | reefid)")
       } else if (length(selected_covar) == 0 && length(unique(test_reefid$reefid)) == 1) {
        as.formula("COUNT ~ 1")
      } else if (length(selected_covar) != 0 && length(unique(test_reefid$reefid)) == 1) {
        as.formula( paste("COUNT ~ 1 +", paste(selected_covar, collapse = " + ")))
      } else {
       as.formula(paste("COUNT ~ 1 + (1 | reefid) +", paste(selected_covar, collapse = " + ")))
  }

    ## Fit FRK model
    M <- tryCatch(
     {

    # Track which formula is actually used (may change in fallback)
    actual_formula <- model_formula
    fitting_method <- "TMB"

    # Precision ridge value (0 = disabled, default)
    ridge_val <- if (exists("PRECISION_RIDGE", envir = .GlobalEnv)) {
      as.numeric(get("PRECISION_RIDGE", envir = .GlobalEnv))
    } else { 0 }
    if (is.na(ridge_val)) ridge_val <- 0
    if (ridge_val > 0) {
      cat(sprintf("  [CONFIG] Precision ridge regularization enabled: %.2e\n", ridge_val))
    }

    # Attempt 1: Full model with TMB method
    M <- tryCatch({
      reefCloudPackage::FRK_regularized(
        f = model_formula,
        data = list(obj_frk$STObj),
        BAUs = obj_frk$ST_BAUs,
        basis = obj_frk$basis,
        response = "binomial",
        link = "logit",
        K_type = "precision",
        method = "TMB",
        est_error = FALSE,
        ridge = ridge_val
      )
    }, error = function(e) {

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
        M_fallback <- tryCatch({
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

          fitting_method <<- "EM"  # Update method tracking

          reefCloudPackage::FRK_regularized(
            f = model_formula,
            data = list(obj_frk$STObj),
            BAUs = obj_frk$ST_BAUs,
            basis = obj_frk$basis,
            response = "binomial",
            link = "logit",
            K_type = "precision",
            method = "EM",
            est_error = FALSE,
            ridge = ridge_val
          )
        }, error = function(e2) {

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

            actual_formula <<- simple_formula  # Update formula tracking
            fitting_method <<- "TMB-intercept"  # Update method tracking

            tryCatch({
              reefCloudPackage::FRK_regularized(
                f = simple_formula,
                data = list(obj_frk$STObj),
                BAUs = obj_frk$ST_BAUs,
                basis = obj_frk$basis,
                response = "binomial",
                link = "logit",
                K_type = "precision",
                method = "TMB",
                est_error = FALSE,
                ridge = ridge_val
              )
            }, error = function(e3) {
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
            })
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
        })

        return(M_fallback)

      } else {
        # Not a singular matrix error, re-throw
        stop(e$message)
      }
    })

    # Update status (wrapped in tryCatch to prevent crash)
    tryCatch({
      old_item_name <- get_status_name(4, "FRK_fit")
        if (!str_detect(old_item_name, "\\[")) {
        new_item_name = paste(old_item_name,"[",i," / ", N,"]")
        } else{
        new_item_name <- str_replace(old_item_name, "\\[([^\\]]*)\\]", paste("[",i," / ", N,"]"))
        }
    }, error = function(e) NULL)

    M  # Return M from the tryCatch expression

     },
     error = function(e) {
       cat(sprintf("  [WARN] FRK model fitting failed for %s: %s: %s\n",
                   FOCAL_TIER, TIER, conditionMessage(e)))
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
    pred <- tryCatch({
      FRK::predict(M, type = "mean", nsim = 1000)
    }, error = function(e) {
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
    })

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
      dplyr::mutate(fYEAR = obj_frk$ST_BAUs@data$fYEAR,
                    Tier5 = obj_frk$ST_BAUs@data$Tier5,
                    id_loc = row_number()) %>%
      tidyr::pivot_longer(!c(fYEAR, Tier5, id_loc), names_to = "draw", values_to = "pred") %>%
      dplyr::mutate(model_name = "FRK")

    # Ensure type consistency for join - convert both to factor with same levels
    tier.sf.joined$Tier5 <- as.factor(tier.sf.joined$Tier5)
    post_dist_df$Tier5 <- factor(post_dist_df$Tier5, levels = levels(tier.sf.joined$Tier5))

    # Extract geometry separately (drop geometry to avoid sf join issues)
    tier_geom <- tier.sf.joined %>%
      dplyr::select(Tier5, geometry) %>%
      dplyr::distinct(Tier5, .keep_all = TRUE)

    pred_sum_sf <- post_dist_df %>% group_by(fYEAR, Tier5) %>%
      ggdist::median_hdci(pred) %>%
      dplyr::left_join(sf::st_drop_geometry(tier_geom), by = "Tier5") %>%
      dplyr::left_join(tier_geom %>% dplyr::select(Tier5, geometry), by = "Tier5") %>%
      sf::st_as_sf() %>%
      dplyr::mutate(Unc = .upper - .lower,
                    Tier5_fYEAR = paste0(Tier5, fYEAR))

  #   ##############################
  #   #### Save outputs
  #   ##############################
    # NB: this used to be wrapped in status::status_try_catch, which silently
    # swallowed every error here (it was hitting an internal `if (debug_mode)`
    # check with a NULL `debug_mode`, breaking error handling) and as a result
    # no tier ever produced an .RData file in /data/modelled. We now use a
    # plain tryCatch and surface the error message via cat() so future
    # failures are visible in the run log.
    save_err <- tryCatch({
    group_slug <- gsub("[ /]", "_", GROUP)
    model_file <- paste0(DATA_PATH, "modelled/", "FRK_", group_slug, "_", FOCAL_TIER, "_", TIER, ".RData")
    saveRDS(
      list(
        group = GROUP,
        form = actual_formula,  # Save the formula that was actually used
        fitting_method = fitting_method,  # Track which method succeeded
        pred_sum_sf = pred_sum_sf,
        post_dist_df = post_dist_df,
        data.grp.tier = data.grp.tier,
        M = M
      ),
      file = model_file
    )

    # Sync this tier's model to S3 immediately (intermediate checkpoint)
    data_path_env <- Sys.getenv("ORIGINAL_DATA_PATH", unset = "")
    if (data_path_env != "" && grepl("^s3://", data_path_env)) {
      s3_dest <- paste0(data_path_env, "/modelled/")
      sync_cmd <- sprintf("aws s3 cp '%s' '%s' --region ap-southeast-2 --only-show-errors 2>&1",
                          model_file,
                          paste0(s3_dest, basename(model_file)))
      sync_result <- suppressWarnings(system(sync_cmd, intern = TRUE, ignore.stderr = FALSE))
      if (length(sync_result) > 0 && any(grepl("error|Error|ERROR", sync_result))) {
        message("S3 sync warning for ", FOCAL_TIER, ": ", TIER, " - ", paste(sync_result, collapse = " "))
      } else {
        message("Checkpoint: Synced ", basename(model_file), " to S3 (",
                round(file.size(model_file)/1024/1024, 1), " MB)")
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
        method = fitting_method,  # "TMB", "EM", or "TMB-intercept"
        n_sites = length(unique(data.grp.tier.ready$Tier5)),
        n_years = length(unique(data.grp.tier.ready$fYEAR)),
        n_obs = nrow(data.grp.tier.ready),
        formula = deparse(actual_formula),
        model_file = basename(model_file)
      )
    }

    # Update status (guard against missing/empty status name)
    tryCatch({
      old_item_name <- get_status_name(4, "FRK_saved")
      if (length(old_item_name) > 0 && nzchar(old_item_name)) {
        if (!str_detect(old_item_name, "\\[")) {
          new_item_name = paste(old_item_name,"[",i," / ", N,"]")
        } else{
          new_item_name <- str_replace(old_item_name, "\\[([^\\]]*)\\]", paste("[",i," / ", N,"]"))
        }
      }
    }, error = function(e) NULL)  # Ignore status display errors

    NULL  # tryCatch body return
   }, error = function(e) e)
   if (inherits(save_err, "error")) {
     msg <- paste0("Save block failed for ", FOCAL_TIER, " = ", TIER,
                   ": ", save_err$message)
     cat("  [WARN] type5: ", msg, "\n", sep = "")
   }

   # ---- Cleanup: free large objects before next tier iteration ----
   rm_vars <- c("M", "pred", "post_dist_df", "pred_sum_sf", "obj_frk",
                "HexPred_sf", "HexPred_reefid2", "full_cov_raw",
                "data.grp.tier", "data.grp.tier.ready", "tier.sf.joined",
                "selected_covar", "test_reefid", "step1_data", "step2_data",
                "step3_data", "step4_data")
   for (v in rm_vars) {
     if (exists(v, inherits = FALSE)) rm(list = v)
   }
   gc()

   }
}
