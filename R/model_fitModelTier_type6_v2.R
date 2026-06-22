#' @title Fit model at tier level
#' @description Fits INLA model to data at tier level when the number of observations is low
#' @param data.grp.not.enough data with low replication at the tier level
#' @param tier.sf covariates shapefile
#' @examples model_fitModelTier_type6()
#' @author Julie Vercelloni
#' @export
model_fitModelTier_type6_v2 <- function(data.grp.not.enough, tier.sf) {
  if (!exists("log_pipeline_step")) {
    log_pipeline_step <- function(...) invisible(NULL)
  }

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
  FOCAL_TIER <- paste0("Tier", as.numeric(BY_TIER) - 1)
  data.grp <- data.grp.not.enough

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

    # Early temporal validation - INLA requires at least 2 time points
    n_years <- length(unique(data.grp.tier$REPORT_YEAR))
    if (n_years < 2) {
      msg <- paste0(
        "Insufficient temporal coverage for ", FOCAL_TIER, ": ", TIER, ". ",
        "Found ", n_years, " year(s), but INLA requires >=2 years for temporal modeling. ",
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
    log_pipeline_step(2, "Join covariates to tier lookup", list())

    tier.sf.joined <- reefCloudPackage::join_covariates_to_tier_lookup(tier.sf, i, N) %>% #
      dplyr::filter(!!sym(FOCAL_TIER) == TIER)

    cat(paste0("  Joined tier rows: ", nrow(tier.sf.joined), "\n"))
    cat(paste0("  Columns: ", paste(names(tier.sf.joined), collapse = ", "), "\n"))
    cat(paste0("  Is sf object: ", inherits(tier.sf.joined, "sf"), "\n\n"))

    # save_debug_csv(sf::st_drop_geometry(tier.sf.joined), debug_dir, "tier_joined", TIER, FOCAL_TIER, "02")
    #
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
    # cat("File loaded: ", files, "\n", sep = "")
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
    if (is.null(geom_col_qc)) geom_col_qc <- "geometry"

    cat(paste0("  97.5% quantile max_cyc: ", round(out_cycl, 3), "\n"))
    cat(paste0("  97.5% quantile max_dhw: ", round(out_dhw, 3), "\n\n"))

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

    ## Process HexPred_sf

    HexPred_reefid2 <- HexPred_sf |>
      dplyr::group_by(Tier5, fYEAR) |>
      dplyr::filter(dplyr::row_number() == 1) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ replace(.x, is.na(.x), 0))) |>
      sf::st_as_sf(sf_column_name = "geometry")

    log_pipeline_step(8, "HexPred_reefid2 ready", list(
      "Rows" = nrow(HexPred_reefid2),
      "Is sf object" = inherits(HexPred_reefid2, "sf"),
      "Class" = paste(class(HexPred_reefid2), collapse = ", ")
    ))

    #--- Filter observations outside Tier5
    log_pipeline_step(9, "Filter observations outside Tier5", list(
      "Observations before filtering" = nrow(data.grp.tier)
    ))

    data.grp.tier.ready <- reefCloudPackage::rm_obs_outside(data.grp.tier, HexPred_reefid2, i, N)

    cat(paste0("  Observations after filtering: ", nrow(data.grp.tier.ready), "\n"))

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
    # debug_dir, "data_ready_for_inla", TIER, FOCAL_TIER, "12"
    # )


    #--- Prepare model objects
    log_pipeline_step(11, "Call inla_prep() to prepare INLA model inputs", list(
      "Data rows" = nrow(data.grp.tier.ready),
      "HexPred_reefid2 rows" = nrow(HexPred_reefid2),
      "HexPred_reefid2 is sf" = inherits(HexPred_reefid2, "sf")
    ))


    # Call inla_prep() with error handling
    obj_inla <- tryCatch(
      {
        reefCloudPackage::inla_prep(data.grp.tier.ready, HexPred_reefid2, i, N)
      },
      error = function(e) {
        msg <- paste0(
          "INLA preparation failed for ", FOCAL_TIER, ": ", TIER, ". ",
          "Error: ", e$message, ". ",
          "Skipping tier."
        )
        cat(paste0("  ERROR: ", msg, "\n\n"))
        status::status_try_catch(
          warning(msg),
          stage_ = 4,
          name_ = "INLA prep error",
          item_ = "type6_inla_prep_error"
        )
        return(NULL)
      }
    )

    # CRITICAL VALIDATION: Check inla_prep() return value and components
    if (is.null(obj_inla)) {
      # Error already logged in tryCatch above
      next
    }


    #--- Build formula
    log_pipeline_step(12, "Build model formula", list(
      "Number of covariates" = length(selected_covar),
      "Number of unique reefids" = length(unique(test_reefid$reefid))
    ))

    if (length(selected_covar) == 0) {
      formula_string <- paste(
        "y ~ -1 + b0 +",
        "f(year_id, model = 'rw1') +",
        "f(Site, model = 'iid') +",
        "f(Transect, model = 'iid')"
      )
    } else {
      formula_string <- paste(
        "y ~ -1 + b0 +",
        "f(year_id, model = 'rw1') +",
        paste(selected_covar, collapse = " + "), "+",
        "f(Site, model = 'iid') +",
        "f(Transect, model = 'iid')"
      )
    }

    model_formula <- as.formula(formula_string)

    cat(paste0("  Initial formula: ", deparse(model_formula), "\n"))

    #--- Test for rank deficiencies (collinear covariates)
    log_pipeline_step(13, "Check for rank deficiencies", list(
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

    #--- Fit INLA model
    log_pipeline_step(14, "Fit INLA model", list(
      "Response" = "binomial",
      "Link" = "logit"
    ))

    # Track which formula and method were actually used
    actual_formula <- model_formula
    fitting_method <- "INLA"

    fit_inla <- function(formula, obj_inla) {
      stk <- obj_inla$stk.full

      INLA::inla(
        formula,
        family = "binomial",
        Ntrials = TOTAL,
        data = INLA::inla.stack.data(stk),
        control.predictor = list(
          A = INLA::inla.stack.A(stk),
          compute = TRUE,
          link = 1
        ),
        control.compute = list(config = TRUE),
        silent = 2L
      )
    }

    M <- tryCatch(
      {
        fit_inla(model_formula, obj_inla)
      },
      error = function(e) {
        # ---- Fallback 1: simplify model if covariates exist ----
        if (!is.null(selected_covar) && length(selected_covar) > 0) {
          message(paste0(
            "INLA failed for ", FOCAL_TIER, ": ", TIER, ". ",
            "Error: ", e$message, ". ",
            "Trying year-only model..."
          ))

          simple_formula <-
            y ~ -1 + b0 + f(year_id, model = "rw1") +
            f(Site, model = "iid") +
            f(Transect, model = "iid")

          actual_formula <<- simple_formula
          fitting_method <<- "year-only"

          res <- tryCatch(
            {
              fit_inla(simple_formula, obj_inla)
            },
            error = function(e2) {
              warning(paste0(
                "All INLA strategies failed for ", FOCAL_TIER, ": ", TIER, ". ",
                "Final error: ", e2$message, ". ",
                "Skipping tier."
              ))

              NULL
            }
          )

          return(res)
        } else {
          # ---- No simplification possible → skip tier ----
          warning(paste0(
            "INLA failed for ", FOCAL_TIER, ": ", TIER, ". ",
            "No covariates to remove or already minimal model. ",
            "Skipping tier. Error: ", e$message
          ))

          next
        }
      }
    )

    #   ##############################
    #   #### Predict & summarise (FRK approach from type5_v3)
    #   ##############################

    log_pipeline_step(15, "Generate predictions (500 simulations)", list())

    ## ================================
    ## INLA POSTERIOR SAMPLING + PREDICTIONS
    ## ================================

    index_pred <- INLA::inla.stack.index(stack = obj_inla$stk.full, tag = "pred")$data

    n_samples <- 500

    samples <- tryCatch(
      {
        INLA::inla.posterior.sample(n_samples, M)
      },
      error = function(e) {
        warning(paste0(
          "Posterior sampling failed for ", FOCAL_TIER, ": ", TIER, ". ",
          e$message, ". Skipping tier."
        ))
        return(NULL)
      }
    )

    if (is.null(samples)) next

    # ---- Extract latent samples ----
    latent_samples <- sapply(samples, function(x) {
      x$latent[index_pred]
    })

    # ---- Build posterior distribution dataframe ----
    post_dist_df <- as.data.frame(latent_samples) |>
      dplyr::mutate(
        Tier5 = HexPred_reefid2$Tier5,
        fYEAR = HexPred_reefid2$fYEAR,
        id_loc = seq_len(nrow(HexPred_reefid2))
      ) |>
      tidyr::pivot_longer(
        cols = -c(Tier5, fYEAR, id_loc),
        names_to = "draw",
        values_to = "eta"
      ) |>
      dplyr::mutate(
        pred = plogis(eta),
        model_name = "INLA"
      )


    # ---- Tier-level summary ----
    tier.sf.joined$Tier5 <- as.factor(tier.sf.joined$Tier5)

    pred_sum_sf <- post_dist_df |>
      dplyr::group_by(Tier5, fYEAR) |>
      ggdist::median_hdci(pred) |>
      dplyr::inner_join(
        tier.sf.joined |> dplyr::select(geometry, Tier5),
        by = "Tier5"
      ) |>
      sf::st_as_sf(sf_column_name = "geometry") |>
      dplyr::mutate(
        Unc = .upper - .lower,
        Tier5_fYEAR = paste0(Tier5, fYEAR)
      )

    #   ##############################
    #   #### Save outputs
    #   ##############################
    group_slug <- gsub("[ /]", "_", GROUP)
    model_file <- paste0(DATA_PATH, "modelled/", "INLA_", group_slug, "_", FOCAL_TIER, "_", TIER, ".RData")

    log_pipeline_step(16, "Save model outputs", list(
      "Output file" = basename(model_file)
    ))

    saveRDS(
      list(
        group = GROUP,
        form = model_formula,
        data.grp.tier = obj_inla$data.sub,
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
    tryCatch(
      {
        old_item_name <- get_status_name(4, "INLA_saved_type6")
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

    # ---- Cleanup: free large objects before next tier iteration ----
    rm_vars <- c(
      "M", "samples", "post_dist_df", "pred_sum_sf",
      "HexPred_sf", "full_cov_raw", "obj_inla",
      "data.grp.tier", "data.grp.tier.ready", "tier.sf.joined",
      "selected_covar"
    )
    for (v in rm_vars) {
      if (exists(v, inherits = FALSE)) rm(list = v)
    }
    gc()
  }
}
