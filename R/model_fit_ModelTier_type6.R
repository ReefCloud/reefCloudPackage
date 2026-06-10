#' @title Fit model at tier level
#' @description Fits FRK model to data at tier level when the number of observations is low
#' @param data.grp.not.enough data with low replication at the tier level
#' @param tier.sf covariates shapefile
#' @examples model_fitModelTier_type6()
#' @author Julie Vercelloni
#' @export
model_fitModelTier_type6 <- function(data.grp.not.enough, tier.sf) {
  # Fetch GROUP from the global environment (set by model_fitModelTier via <<-).
  # Package namespace scoping can prevent free-variable lookup from reaching
  # .GlobalEnv, so we resolve it explicitly here.
  GROUP <- get("GROUP", envir = .GlobalEnv)

  # Print version number
  version_file <- system.file("VERSION", package = "reefCloudPackage")
  if (file.exists(version_file)) {
    version <- readLines(version_file, n = 1)
    cat(paste0("\n### reefCloudPackage VERSION: ", version, "\n"))
  }

  # Define spatial scale
  FOCAL_TIER <- paste0('Tier', as.numeric(BY_TIER) - 1)
  data.grp <- data.grp.not.enough

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

  # Debug directory disabled to reduce S3 storage usage
  # debug_dir <- paste0(DATA_PATH, "debug/type6/")
  # if (!dir.exists(debug_dir)) {
  #   dir.create(debug_dir, recursive = TRUE)
  # }

  ######################
  ###################### START THE LOOP

  for (i in seq_along(tiers)) {

    TIER <<- as.character(tiers[i])

    cat(paste0("\n", strrep("#", 80), "\n"))
    cat(paste0("### PROCESSING TIER ", i, " of ", N, ": ", FOCAL_TIER, " = ", TIER, "\n"))
    cat(paste0(strrep("#", 80), "\n\n"))

    #--- Filter input data
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

    # Early temporal validation - FRK requires at least 2 time points
    n_years <- length(unique(data.grp.tier$REPORT_YEAR))
    if (n_years < 2) {
      msg <- paste0(
        "Insufficient temporal coverage for ", FOCAL_TIER, ": ", TIER, ". ",
        "Found ", n_years, " year(s), but FRK requires >=2 years for temporal modeling. ",
        "Skipping tier."
      )
      cat(paste0("  WARNING: ", msg, "\n\n"))
      status::status_try_catch(
        warning(msg),
        stage_ = 4,
        name_ = "Insufficient temporal data",
        item_ = "type6_temporal_check"
      )
      next
    }

    # Debug saves disabled
    # save_debug_csv(data.grp.tier, debug_dir, "data_filtered", TIER, FOCAL_TIER, "01")
    #
    #--- Join covariates
    # log_pipeline_step(2, "Join covariates to tier lookup", list())

    tier.sf.joined <- reefCloudPackage::join_covariates_to_tier_lookup(tier.sf, i, N) %>%
      dplyr::filter(!!sym(FOCAL_TIER) == TIER)

    cat(paste0("  Joined tier rows: ", nrow(tier.sf.joined), "\n"))
    cat(paste0("  Columns: ", paste(names(tier.sf.joined), collapse = ", "), "\n"))
    cat(paste0("  Is sf object: ", inherits(tier.sf.joined, "sf"), "\n\n"))

    # save_debug_csv(sf::st_drop_geometry(tier.sf.joined), debug_dir, "tier_joined", TIER, FOCAL_TIER, "02")
    # 
    #--- Load and filter predictive layers
    # log_pipeline_step(3, "Load and filter predictive layers", list())

    full_cov_raw <- reefCloudPackage::load_predictive_layers(i, N) %>%
      dplyr::filter(Tier5 %in% tier.sf.joined$Tier5) %>%
      dplyr::rename(fYEAR = year) %>%
      dplyr::filter(
        between(fYEAR,
                min(data.grp.tier$REPORT_YEAR),
                max(data.grp.tier$REPORT_YEAR))
      )

    cat(paste0("  Raw covariate rows: ", nrow(full_cov_raw), "\n"))
    cat(paste0("  Is sf object: ", inherits(full_cov_raw, "sf"), "\n"))
    cat(paste0("  Unique Tier5: ", length(unique(full_cov_raw$Tier5)), "\n"))
    cat(paste0("  Unique years: ", length(unique(full_cov_raw$fYEAR)), "\n\n"))

    #--- Apply control quality on extreme values
    log_pipeline_step(4, "Apply QC on extreme values", list())

    out_cycl <- quantile(full_cov_raw$max_cyc, probs = 0.975)
    out_dhw  <- quantile(full_cov_raw$max_dhw, probs = 0.975)
    # Get geometry column to exclude from across() operations
    geom_col_qc <- attr(full_cov_raw, "sf_column")
    if (is.null(geom_col_qc)) geom_col_qc <- "geometry"

    cat(paste0("  97.5% quantile max_cyc: ", round(out_cycl, 3), "\n"))
    cat(paste0("  97.5% quantile max_dhw: ", round(out_dhw, 3), "\n\n"))

    HexPred_sf <- full_cov_raw %>%
      dplyr::mutate(As.Data = ifelse(Tier5 %in% data.grp.tier$Tier5, "Yes", "No")) %>%
      dplyr::mutate(across(matches("^max_cyc.*") & -all_of(geom_col_qc),
                           ~ ifelse(.x >= out_cycl & As.Data == "No", NA, .x))) %>%
      dplyr::mutate(across(matches("^max_dhw.*") & -all_of(geom_col_qc),
                           ~ ifelse(.x >= out_dhw & As.Data == "No", NA, .x)))

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

    cat(paste0("  Selected covariates: ", paste(selected_covar, collapse = ", "), "\n"))
    cat(paste0("  Number of covariates: ", length(selected_covar), "\n\n"))

    ## Scale covariates
    log_pipeline_step(7, "Scale covariates", list())

    HexPred_sf <- HexPred_sf %>%
      dplyr::mutate(across(
       matches("^severity.*|^max.*"),
      ~ {
        s <- sd(., na.rm = TRUE)
        if (is.na(s) || s == 0) . else as.numeric((. - mean(., na.rm = TRUE)) / s)
      }
     ))

    cat(paste0("  HexPred_sf still sf: ", inherits(HexPred_sf, "sf"), "\n"))
    cat(paste0("  HexPred_sf still has geometry: ", !is.null(sf::st_geometry(HexPred_sf)), "\n\n"))

    #--- Create reefid
    log_pipeline_step(8, "Create reefid using make_reefid()", list())

    covs.hexpred_tier_sf_v2_prep <- reefCloudPackage::make_reefid(tier.sf.joined, HexPred_sf, reef_layer.sf, i, N)

    cat(paste0("  make_reefid output rows: ", nrow(covs.hexpred_tier_sf_v2_prep), "\n"))
    cat(paste0("  Is sf object: ", inherits(covs.hexpred_tier_sf_v2_prep, "sf"), "\n"))
    cat(paste0("  Columns: ", paste(names(covs.hexpred_tier_sf_v2_prep), collapse = ", "), "\n"))
    cat(paste0("  Has reefid column: ", "reefid" %in% names(covs.hexpred_tier_sf_v2_prep), "\n"))
    if ("reefid" %in% names(covs.hexpred_tier_sf_v2_prep)) {
      n_na_reefid <- sum(is.na(covs.hexpred_tier_sf_v2_prep$reefid))
      cat(paste0("  NA reefid count: ", n_na_reefid, " (", round(100*n_na_reefid/nrow(covs.hexpred_tier_sf_v2_prep), 2), "%)\n"))
      cat(paste0("  Unique reefids: ", length(unique(covs.hexpred_tier_sf_v2_prep$reefid[!is.na(covs.hexpred_tier_sf_v2_prep$reefid)])), "\n"))
    }
    cat("\n")

    # save_debug_csv(
    # sf::st_drop_geometry(covs.hexpred_tier_sf_v2_prep),
    # debug_dir, "make_reefid_output", TIER, FOCAL_TIER, "08"
    # )

    #--- Merge reefid with covariates
    log_pipeline_step(9, "Summarise reefid by Tier5", list())

    HexPred_reefid <- covs.hexpred_tier_sf_v2_prep %>%
      dplyr::group_by(Tier5) %>%
      dplyr::summarise(reefid = paste0(reefid, collapse = "_")) %>%
      dplyr::ungroup()

    cat(paste0("  HexPred_reefid rows: ", nrow(HexPred_reefid), "\n"))
    cat(paste0("  Is sf object: ", inherits(HexPred_reefid, "sf"), "\n"))
    cat(paste0("  Has geometry: ", "geometry" %in% names(HexPred_reefid), "\n\n"))

    # save_debug_csv(
    # sf::st_drop_geometry(HexPred_reefid),
    # debug_dir, "HexPred_reefid_summary", TIER, FOCAL_TIER, "09"
    # )

    # Use inner_join to keep only Tier5s that have a reefid (i.e., intersect with reef layer).
    # This matches Julie's approach: only reef-containing hexagons become BAUs for FRK.
    log_pipeline_step(10, "INNER JOIN reefid to HexPred_sf (reef-only BAUs)", list(
      "HexPred_sf rows before" = nrow(HexPred_sf),
      "HexPred_reefid rows" = nrow(HexPred_reefid)
    ))

    HexPred_reefid2 <- dplyr::inner_join(
        HexPred_sf |> data.frame(),
        sf::st_drop_geometry(HexPred_reefid),
        by = "Tier5"
      ) |>
      dplyr::group_by(Tier5, fYEAR) |>
      dplyr::filter(dplyr::row_number() == 1) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ replace(.x, is.na(.x), 0))) |>
      sf::st_as_sf(sf_column_name = "geometry")

    log_pipeline_step(11, "HexPred_reefid2 AFTER inner_join", list(
      "Rows" = nrow(HexPred_reefid2),
      "Is sf object" = inherits(HexPred_reefid2, "sf"),
      "Has reefid column" = "reefid" %in% names(HexPred_reefid2),
      "Class" = paste(class(HexPred_reefid2), collapse = ", ")
    ))

    #--- Filter observations outside Tier5
    log_pipeline_step(12, "Filter observations outside Tier5", list(
      "Observations before" = nrow(data.grp.tier)
    ))

    data.grp.tier.ready <- reefCloudPackage::rm_obs_outside(data.grp.tier, HexPred_reefid2, i, N)

    cat(paste0("  Observations after: ", nrow(data.grp.tier.ready), "\n"))

    ## Skip if more than 30% of observations are outside Tier5 cells (Julie's rule).
    ## Mirrors the type5 path so both FRK paths gate consistently.

    diff_perc <- ((nrow(data.grp.tier) - nrow(data.grp.tier.ready)) / nrow(data.grp.tier)) * 100
    cat(paste0("  Removed: ", nrow(data.grp.tier) - nrow(data.grp.tier.ready), " (", round(diff_perc, 2), "%)\n\n"))

    if (diff_perc > 30) {
      msg <- paste0(round(diff_perc, 1), "% of data locations are outside Tier5 cells for ", FOCAL_TIER, ": ", TIER, " — skipping.")
      cat(paste0("  WARNING: ", msg, "\n"))
      message(msg)
      next
    }

    #--- Check minimum observation threshold
    log_pipeline_step(12.5, "Validate minimum observations", list(
      "Observations after filtering" = nrow(data.grp.tier.ready),
      "Minimum required" = 20
    ))

    n_obs_after_filter <- nrow(data.grp.tier.ready)

    if (n_obs_after_filter < 20) {
      msg <- paste0(
        "Insufficient observations for ", FOCAL_TIER, ": ", TIER, ". ",
        "Found ", n_obs_after_filter, " observations after filtering. ",
        "FRK models require at least 20 observations for stable estimation. ",
        "Skipping tier."
      )
      cat(paste0("  ERROR: ", msg, "\n\n"))
      status::status_try_catch(
        warning(msg),
        stage_ = 4,
        name_ = "Insufficient observations",
        item_ = "min_obs_threshold_type6"
      )
      next
    }

    cat(paste0("  Observation count check passed: ", n_obs_after_filter, " observations\n\n"))

    #--- Final spatiotemporal coverage validation (after all filtering)
    log_pipeline_step(12.7, "Validate spatiotemporal coverage after filtering", list(
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
        item_ = "type6_temporal_check_post_filter"
      )
      next
    }

    if (n_sites_final < 2) {
      msg <- paste0(
        "Insufficient spatial coverage after filtering for ", FOCAL_TIER, ": ", TIER, ". ",
        "Found ", n_sites_final, " site(s) (Tier5) after removing observations outside tier boundaries. ",
        "FRK requires at least 2 spatial locations for spatial modeling. ",
        "Skipping tier."
      )
      cat(paste0("  ERROR: ", msg, "\n\n"))
      status::status_try_catch(
        warning(msg),
        stage_ = 4,
        name_ = "Insufficient spatial coverage after filtering",
        item_ = "type6_spatial_check_post_filter"
      )
      next
    }

    cat(paste0("  ✓ Spatiotemporal coverage check passed: ", n_years_final, " years × ", n_sites_final, " sites\n\n"))

    # save_debug_csv(
    # data.grp.tier.ready,
    # debug_dir, "data_ready_for_frk", TIER, FOCAL_TIER, "12"
    # )

     ## Test if more than one reef in the final data
    log_pipeline_step(13, "Filter HexPred_reefid2 to match data locations", list())

    test_reefid <- HexPred_reefid2 %>% filter(Tier5 %in% data.grp.tier.ready$Tier5)

    cat(paste0("  test_reefid rows: ", nrow(test_reefid), "\n"))
    cat(paste0("  Unique reefids: ", length(unique(test_reefid$reefid)), "\n\n"))

    #--- Prepare model objects
    log_pipeline_step(14, "Call frk_prep() to prepare FRK model inputs", list(
      "Data rows" = nrow(data.grp.tier.ready),
      "HexPred_reefid2 rows" = nrow(HexPred_reefid2),
      "HexPred_reefid2 is sf" = inherits(HexPred_reefid2, "sf")
    ))

    # Call frk_prep() with error handling
    # frk_prep() no longer wraps itself in status_try_catch, so errors propagate naturally
    basis_res <- if (exists("BASIS_RESOLUTION")) as.integer(BASIS_RESOLUTION) else 3L
    obj_frk <- tryCatch(
      {
        reefCloudPackage::frk_prep(data.grp.tier.ready, HexPred_reefid2, i, N, basis_resolution = basis_res)
      },
      error = function(e) {
        msg <- paste0(
          "FRK preparation failed for ", FOCAL_TIER, ": ", TIER, ". ",
          "Error: ", e$message, ". ",
          "Skipping tier."
        )
        cat(paste0("  ERROR: ", msg, "\n\n"))
        status::status_try_catch(
          warning(msg),
          stage_ = 4,
          name_ = "FRK prep error",
          item_ = "type6_frk_prep_error"
        )
        return(NULL)
      }
    )

    # CRITICAL VALIDATION: Check frk_prep() return value and components
    if (is.null(obj_frk)) {
      # Error already logged in tryCatch above
      next
    }

    # Check for NULL components
    missing_components <- c()
    if (is.null(obj_frk$ST_BAUs)) missing_components <- c(missing_components, "ST_BAUs")
    if (is.null(obj_frk$STObj)) missing_components <- c(missing_components, "STObj")
    if (is.null(obj_frk$basis)) missing_components <- c(missing_components, "basis")

    if (length(missing_components) > 0) {
      msg <- paste0(
        "FRK preparation failed for ", FOCAL_TIER, ": ", TIER, ". ",
        "frk_prep() returned list with NULL components: ", paste(missing_components, collapse=", "), ". ",
        "Review 'Prep FRK objects' error messages above for root cause. ",
        "This usually indicates validation failure inside frk_prep() that was caught by outer try-catch. ",
        "Skipping tier."
      )
      cat(paste0("  ERROR: ", msg, "\n\n"))
      cat(paste0("  DIAGNOSTIC: obj_frk class=", class(obj_frk)[1],
                 ", length=", length(obj_frk),
                 ", names=", paste(names(obj_frk), collapse=", "), "\n"))
      status::status_try_catch(
        warning(msg),
        stage_ = 4,
        name_ = "FRK prep incomplete",
        item_ = "type6_frk_prep_incomplete"
      )
      next
    }

    cat(paste0("  frk_prep returned successfully\n"))
    cat(paste0("  ST_BAUs@data rows: ", nrow(obj_frk$ST_BAUs@data), "\n"))
    cat(paste0("  ST_BAUs@data has reefid: ", "reefid" %in% names(obj_frk$ST_BAUs@data), "\n"))
    if ("reefid" %in% names(obj_frk$ST_BAUs@data)) {
      cat(paste0("  ST_BAUs unique reefids: ", length(unique(obj_frk$ST_BAUs@data$reefid)), "\n"))
    }

    #--- Validate basis function count
    log_pipeline_step(14.5, "Validate basis functions", list(
      "Basis functions" = nrow(obj_frk$basis),
      "Observations" = nrow(data.grp.tier.ready)
    ))

    n_basis <- nrow(obj_frk$basis)
    n_obs <- nrow(data.grp.tier.ready)
    basis_ratio <- n_basis / n_obs

    cat(paste0("  Basis functions: ", n_basis, ", Observations: ", n_obs,
               ", Ratio: ", round(basis_ratio, 3), "\n"))

    if (basis_ratio > 0.5) {
      msg <- paste0(
        "Basis function overspecification for ", FOCAL_TIER, ": ", TIER, ". ",
        "Basis functions (", n_basis, ") exceed 50% of observations (", n_obs, "). ",
        "This typically causes fitting failures. ",
        "Skipping tier to avoid numerical instability."
      )
      cat(paste0("  ERROR: ", msg, "\n\n"))
      status::status_try_catch(
        warning(msg),
        stage_ = 4,
        name_ = "Basis overspecification",
        item_ = "basis_overspec_type6"
      )
      next
    }

    cat(paste0("  Basis validation passed: ratio ", round(basis_ratio, 3), " < 0.5\n\n"))

    #--- Build formula
    log_pipeline_step(15, "Build model formula", list(
      "Number of covariates" = length(selected_covar),
      "Number of unique reefids" = length(unique(test_reefid$reefid))
    ))

    # Use FRK formula structure (simpler than INLA, based on type5_v3)
    if (length(selected_covar) == 0 && length(unique(test_reefid$reefid)) > 1) {
        model_formula <- as.formula("COUNT ~ 1 + (1 | reefid)")
    } else if (length(selected_covar) == 0 && length(unique(test_reefid$reefid)) == 1) {
        model_formula <- as.formula("COUNT ~ 1")
    } else if (length(selected_covar) != 0 && length(unique(test_reefid$reefid)) == 1) {
        model_formula <- as.formula(paste("COUNT ~ 1 +", paste(selected_covar, collapse = " + ")))
    } else {
        model_formula <- as.formula(paste("COUNT ~ 1 + (1 | reefid) +", paste(selected_covar, collapse = " + ")))
    }

    cat(paste0("  Initial formula: ", deparse(model_formula), "\n"))

    #--- Test for rank deficiencies (collinear covariates)
    log_pipeline_step(15.5, "Check for rank deficiencies", list(
      "Number of covariates" = length(selected_covar)
    ))

    if (length(selected_covar) > 0) {
      result_rank <- reefCloudPackage::rank_checks(data.grp.tier.ready, HexPred_reefid2, selected_covar)

      if (result_rank$status == "fail") {
        msg <- paste0(
          "Rank deficiency detected for ", FOCAL_TIER, ": ", TIER, ". ",
          "Covariates exhibit perfect collinearity. ",
          "Using simplified formula with rank-deficient covariates removed."
        )
        cat(paste0("  WARNING: ", msg, "\n"))
        status::status_try_catch(
          message(msg),
          stage_ = 4,
          name_ = "Rank deficiency detected",
          item_ = "rank_deficiency_type6"
        )

        # Update formula based on rank_checks result
        model_formula <- as.formula(result_rank$formula)
        cat(paste0("  Updated formula: ", deparse(model_formula), "\n\n"))
      } else {
        cat(paste0("  Rank check passed: no collinearity detected\n\n"))
      }
    } else {
      cat(paste0("  Skipping rank check: no covariates in model\n\n"))
    }

    #--- Fit FRK model (changed from INLA)
    log_pipeline_step(16, "Fit FRK model", list(
      "Method" = "TMB",
      "Response" = "binomial",
      "Link" = "logit"
    ))

    # Track which formula and method were actually used
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

    M <- tryCatch({
      tryCatch({
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
          message(msg)

          # Fallback 1: Try with EM method instead of TMB
          M_fallback <- tryCatch({
            msg_fallback1 <- paste0(
              "Model Fitting: Attempting EM algorithm (more robust, slower) for ",
              FOCAL_TIER, ": ", TIER
            )
            message(msg_fallback1)

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
              message(msg)

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
                warning(msg)
                return(NULL)
              })
            } else {
              # No covariates to remove, can't simplify further
              msg <- paste0(
                "Model Fitting: TMB and EM methods failed for ", FOCAL_TIER, ": ", TIER, ". ",
                "Already using intercept-only model - cannot simplify further. ",
                "Data structure may be incompatible with FRK. Skipping tier."
              )
              warning(msg)
              return(NULL)
            }
          })

          return(M_fallback)

        } else {
          # Not a singular matrix error, re-throw
          stop(e$message)
        }
      })
    },
    error = function(e) {
      cat(sprintf("  [WARN] FRK model fitting failed for %s: %s: %s\n",
                  FOCAL_TIER, TIER, conditionMessage(e)))
      NULL
    }
    )

    cat(paste0("  FRK model fit completed using ", fitting_method, " method\n"))
    cat(paste0("  Final formula: ", deparse(actual_formula), "\n"))
    cat(paste0("  Model object class: ", class(M)[1], "\n\n"))

    # Update status (guard against missing/empty status name)
    tryCatch({
      old_item_name <- get_status_name(4, "FRK_fit_type6")
      if (length(old_item_name) > 0 && nzchar(old_item_name)) {
        if (!str_detect(old_item_name, "\\[")) {
          new_item_name = paste(old_item_name,"[",i," / ", N,"]")
        } else{
          new_item_name <- str_replace(old_item_name, "\\[([^\\]]*)\\]", paste("[",i," / ", N,"]"))
        }
      }
    }, error = function(e) NULL)  # Ignore status display errors

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
        item_ = "invalid_model_object_type6"
      )
      next
    }

  #   ##############################
  #   #### Predict & summarise (FRK approach from type5_v3)
  #   ##############################

  log_pipeline_step(17, "Generate predictions (1000 simulations)", list())

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
        item_ = "sre_prediction_error_type6"
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
    status::status_try_catch(warning(msg), stage_ = 4, name_ = "Prediction is NULL", item_ = "pred_is_null_type6")
    next
  }
  if (is.null(pred$MC)) {
    msg <- paste0(
      "Prediction Failed: Monte Carlo samples missing for ", FOCAL_TIER, ": ", TIER, ". ",
      "Prediction structure invalid. Skipping tier."
    )
    status::status_try_catch(warning(msg), stage_ = 4, name_ = "MC samples missing", item_ = "mc_samples_missing_type6")
    next
  }
  if (is.null(pred$MC$mu_samples)) {
    msg <- paste0(
      "Prediction Failed: Mean samples (mu_samples) missing for ", FOCAL_TIER, ": ", TIER, ". ",
      "Cannot compute posterior distributions. Skipping tier."
    )
    status::status_try_catch(warning(msg), stage_ = 4, name_ = "mu_samples missing", item_ = "mu_samples_missing_type6")
    next
  }

  cat(paste0("  Predictions generated\n"))
  cat(paste0("  Prediction samples: ", nrow(pred$MC$mu_samples), " x ", ncol(pred$MC$mu_samples), "\n"))

  # Check dimension compatibility before mutate
  n_pred_rows <- nrow(pred$MC$mu_samples)
  n_bau_rows <- nrow(obj_frk$ST_BAUs@data)
  if (n_pred_rows != n_bau_rows) {
    msg <- paste0(
      "Prediction Failed: Dimension mismatch for ", FOCAL_TIER, ": ", TIER, ". ",
      "Predictions: ", n_pred_rows, " rows, BAUs: ", n_bau_rows, " rows. ",
      "BAU grid and predictions incompatible - likely BAU temporal filtering issue. Skipping tier."
    )
    status::status_try_catch(warning(msg), stage_ = 4, name_ = "Dimension mismatch", item_ = "dimension_mismatch_type6")
    next
  }

  cat(paste0("  Dimension check passed: ", n_pred_rows, " predictions match ", n_bau_rows, " BAUs\n\n"))

  post_dist_df <- as.data.frame(pred$MC$mu_samples) %>%
    dplyr::mutate(fYEAR = obj_frk$ST_BAUs@data$fYEAR,
                  Tier5 = obj_frk$ST_BAUs@data$Tier5,
                  id_loc = row_number()) %>%
    tidyr::pivot_longer(!c(fYEAR, Tier5, id_loc), names_to = "draw", values_to = "pred") %>%
    dplyr::mutate(model_name = "FRK")

  #--- Summary predictions by Tier5
  # Ensure type consistency for join - convert both to factor with same levels
  tier.sf.joined$Tier5 <- as.factor(tier.sf.joined$Tier5)
  post_dist_df$Tier5 <- factor(post_dist_df$Tier5, levels = levels(tier.sf.joined$Tier5))

    pred_sum_sf <- post_dist_df %>%
      dplyr::group_by(fYEAR, Tier5) %>%
      ggdist::median_hdci(pred) %>%
      dplyr::inner_join(
        tier.sf.joined %>% dplyr::select(geometry, Tier5),
        by = "Tier5"
      ) %>%
      sf::st_as_sf(sf_column_name = "geometry") %>%
      dplyr::mutate(
        Unc = .upper - .lower,
        Tier5_fYEAR = paste0(Tier5, fYEAR)
      )

  #   ##############################
  #   #### Save outputs
  #   ##############################
    group_slug <- gsub("[ /]", "_", GROUP)
    model_file <- paste0(DATA_PATH, "modelled/", "FRK_", group_slug, "_", FOCAL_TIER, "_", TIER, ".RData")
    log_pipeline_step(18, "Save model outputs", list(
      "Output file" = basename(model_file)
    ))

    saveRDS(
      list(
        group = GROUP,
        form = model_formula,
        data.grp.tier = data.grp.tier,
        pred_sum_sf = pred_sum_sf,
        post_dist_df = post_dist_df,
        M = M
      ),
      file = model_file
    )

    cat(paste0("  ✓ Model outputs saved successfully\n\n"))
    cat(paste0("### COMPLETED TIER ", i, " of ", N, ": ", FOCAL_TIER, " = ", TIER, "\n"))
    cat(paste0(strrep("#", 80), "\n\n"))
      # Update status (guard against missing/empty status name)
      tryCatch({
        old_item_name <- get_status_name(4, "FRK_saved_type6")
        if (length(old_item_name) > 0 && nzchar(old_item_name)) {
          if (!str_detect(old_item_name, "\\[")) {
            new_item_name = paste(old_item_name,"[",i," / ", N,"]")
          } else{
            new_item_name <- str_replace(old_item_name, "\\[([^\\]]*)\\]", paste("[",i," / ", N,"]"))
          }
        }
      }, error = function(e) NULL)  # Ignore status display errors

   # ---- Cleanup: free large objects before next tier iteration ----
   rm_vars <- c("M", "pred", "post_dist_df", "pred_sum_sf", "obj_frk",
                "HexPred_sf", "HexPred_reefid2", "full_cov_raw",
                "data.grp.tier", "data.grp.tier.ready", "tier.sf.joined",
                "selected_covar", "test_reefid")
   for (v in rm_vars) {
     if (exists(v, inherits = FALSE)) rm(list = v)
   }
   gc()

   }  
}
