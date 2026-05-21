#' @title Fit model at tier level
#' @description Fits FRK model to monitoring data at a given tier level, including covariate processing, quality control, model fitting and predictions.
#' @param data.grp.enough Data frame with sufficient data for modelling (filtered upstream)
#' @param tier.sf Tier-level shapefile (e.g., Tier3 or Tier4 spatial scale)
#' @examples
#' model_fitModelTier_type5(my_data_filtered, my_tier_sf)
#' @author Julie Vercelloni
#' @export
model_fitModelTier_type5 <- function(data.grp.enough, tier.sf){

  FOCAL_TIER <- paste0('Tier', as.numeric(BY_TIER) - 1)
  data.grp <- data.grp.enough

  tiers <- unique(data.grp[[FOCAL_TIER]])
  N <- length(unique(data.grp[[FOCAL_TIER]]))

  #########################
  #### START LOOP BY TIER
  #########################
  for (i in seq_along(tiers)) {
    TIER <- as.character(tiers[i])

    ## Filter to current tier
    data.grp.tier <- data.grp |>
      dplyr::filter(data.grp[[FOCAL_TIER]] == TIER) |>
      dplyr::select(-COVER) |>
      dplyr::mutate(Tier5 = as.character(Tier5))

    ## Join covariates
    tier.sf.joined <- reefCloudPackage::join_covariates_to_tier_lookup(tier.sf, i, N) |> 
      dplyr::filter(!!sym(FOCAL_TIER) == TIER)

    ## Load covariate layers
    full_cov_raw <- reefCloudPackage::load_predictive_layers(i , N) |> 
      dplyr::filter(Tier5 %in% tier.sf.joined$Tier5) |>
      dplyr::rename(fYEAR = year) |>
      dplyr::filter(between(fYEAR, min(data.grp.tier$REPORT_YEAR), max(data.grp.tier$REPORT_YEAR)))

    ## Apply quality control thresholds
    out_cycl <- quantile(full_cov_raw$max_cyc, probs = 0.975)
    out_dhw  <- quantile(full_cov_raw$max_dhw, probs = 0.975)
    
    HexPred_sf <- full_cov_raw |>
      dplyr::mutate(As.Data = ifelse(Tier5 %in% data.grp.tier$Tier5, "Yes", "No")) |>
      dplyr::mutate(across(matches("^max_cyc.*"), ~ ifelse(.x >= out_cycl & As.Data == "No", NA, .x))) |>
      dplyr::mutate(across(matches("^max_dhw.*"), ~ ifelse(.x >= out_dhw & As.Data == "No", NA, .x)))

    ## Select covariates
    selected_covar <- reefCloudPackage::select_covariates(HexPred_sf, i , N) 

    ## Check for collinearity 
    selected_covar <- reefCloudPackage::filter_non_collinear(HexPred_sf, selected_covar, threshold = 0.7)
    
    ## Add reefid and fill missing years
    covs.hexpred_tier_sf_v2_prep <- reefCloudPackage::make_reefid(tier.sf.joined, HexPred_sf, reef_layer.sf, i , N) 

   HexPred_reefid <- covs.hexpred_tier_sf_v2_prep |>
      dplyr::group_by(Tier5) |>
      dplyr::summarise(reefid = paste0(reefid, collapse = "_")) |>
      ungroup()

    HexPred_reefid2 <- inner_join(HexPred_sf |> data.frame(), HexPred_reefid) |>
      dplyr::group_by(Tier5, fYEAR) |>
      dplyr::filter(row_number() == 1) |>
      dplyr::mutate(across(everything(), ~ replace(.x, is.na(.x), 0))) |>
      sf::st_as_sf(sf_column_name = "geometry")

    ## Remove obs outside covariate grid
    data.grp.tier.ready <- reefCloudPackage::rm_obs_outside(data.grp.tier, HexPred_reefid2, i , N)    

    ## Stop if more than 10% of observations are outside tier5 cells 
    diff_perc <- ((nrow(data.grp.tier) - nrow(data.grp.tier.ready)) / nrow(data.grp.tier)) * 100

     if (diff_perc > 10) {
       msg <- paste(diff_perc, "% of data locations are outside Tier5 cells for", FOCAL_TIER, ":", TIER)
       status:::status_log("ERROR", log_file = log_file, "--Fitting FRK model--", msg = msg)
     next
    }

    ## Test if more than one reef in the final data 
    test_reefid <- HexPred_reefid2 %>% filter(Tier5 %in% data.grp.tier.ready$Tier5)

    ## Prep FRK model inputs
    obj_frk <- reefCloudPackage::frk_prep(data.grp.tier.ready, HexPred_reefid2, i , N)  

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
    status::status_try_catch(
     {
    M <- FRK(
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

    # Update status 
      old_item_name <- get_status_name(4, "FRK_fit")
        if (!str_detect(old_item_name, "\\[")) {
        new_item_name = paste(old_item_name,"[",i," / ", N,"]")
        } else{
        new_item_name <- str_replace(old_item_name, "\\[([^\\]]*)\\]", paste("[",i," / ", N,"]"))
        }
      status:::update_status_name(stage = 4, item = "FRK_fit", name = new_item_name)

     },
     stage_ = 4,
     order_ = 10,
     name_ = "Fit FRK model",
     item_ = "FRK_fit"
   )
  
    ## Handle failed model
    if (length(M) == 0) {
      msg <- paste("Model failed to fit for", FOCAL_TIER, ":", TIER)
      status:::status_log("ERROR", log_file = log_file, "--Fitting FRK model--", msg = msg)
      next
    }

  #   ##############################
  #   #### Predict & summarise
  #   ##############################
    # pred <- FRK::predict(M, type = "mean", nsim = 1000)

    # post_dist_df <- as.data.frame(pred$MC$mu_samples) |>
    #   dplyr::mutate(fYEAR = obj_frk$ST_BAUs@data$fYEAR,
    #                 Tier5 = obj_frk$ST_BAUs@data$Tier5,
    #                 id_loc = row_number()) |>
    #   tidyr::pivot_longer(!c(fYEAR, Tier5, id_loc), names_to = "draw", values_to = "pred") |>
    #   dplyr::mutate(model_name = "FRK")

    # tier.sf.joined$Tier5 <- as.factor(tier.sf.joined$Tier5)

    # pred_sum_sf <- post_dist_df |> group_by(fYEAR, Tier5) |>
    #   ggdist::median_hdci(pred) |>
    #   dplyr::inner_join(tier.sf.joined |> dplyr::select(geometry, Tier5)) |>
    #   sf::st_as_sf(sf_column_name = "geometry") |>
    #   dplyr::mutate(Unc = .upper - .lower,
    #                 Tier5_fYEAR = paste0(Tier5, fYEAR))

  #   ##############################
  #   #### Save outputs
  #   ##############################
    status::status_try_catch(
     {
    saveRDS(
      list(
  #      form = model_formula,
  #      pred_sum_sf = pred_sum_sf,
  #      post_dist_df = post_dist_df,
        data.grp.tier = data.grp.tier,
        M = M
      ),
      file = paste0(DATA_PATH, "modelled/", "FRK_", FOCAL_TIER, "_", TIER, "_", GROUP, ".RData")
    )

    # Update status 
      old_item_name <- get_status_name(4, "FRK_saved")
        if (!str_detect(old_item_name, "\\[")) {
        new_item_name = paste(old_item_name,"[",i," / ", N,"]")
        } else{
        new_item_name <- str_replace(old_item_name, "\\[([^\\]]*)\\]", paste("[",i," / ", N,"]"))
        }
      status:::update_status_name(stage = 4, item = "FRK_saved", name = new_item_name)

     },
     stage_ = 4,
     order_ = 11,
     name_ = "Saved FRK outputs",
     item_ = "FRK_saved"
   )
   }
}
