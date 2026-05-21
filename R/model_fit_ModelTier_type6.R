#' @title Fit model at tier level
#' @description Fits INLA model to data at tier level when the number of observations is low
#' @param data.grp.not.enough data with low replication at the tier level
#' @param tier.sf covariates shapefile
#' @examples model_fitModelTier_type6()
#' @author Julie Vercelloni
#' @export
model_fitModelTier_type6 <- function(data.grp.not.enough, tier.sf) {
  # Define spatial scale
  FOCAL_TIER <- paste0('Tier', as.numeric(BY_TIER) - 1)
  data.grp <- data.grp.not.enough

  tiers <- unique(data.grp[[FOCAL_TIER]])
  N <- length(unique(data.grp[[FOCAL_TIER]]))

  ######################
  ###################### START THE LOOP

  for (i in seq_along(tiers)) {

    TIER <<- as.character(tiers[i])

    #--- Filter input data
    data.grp.tier <- data.grp |>
      dplyr::filter(data.grp[[FOCAL_TIER]] == TIER) |>
      dplyr::select(-COVER) |>
      dplyr::mutate(across(Tier5, as.character))

    #--- Join covariates
    tier.sf.joined <- reefCloudPackage::join_covariates_to_tier_lookup(tier.sf, i, N) |>
      dplyr::filter(!!sym(FOCAL_TIER) == TIER)

    #--- Load and filter predictive layers
    full_cov_raw <- reefCloudPackage::load_predictive_layers(i, N) |>
      dplyr::filter(Tier5 %in% tier.sf.joined$Tier5) |>
      dplyr::rename(fYEAR = year) |>
      dplyr::filter(
        between(fYEAR,
                min(data.grp.tier$REPORT_YEAR),
                max(data.grp.tier$REPORT_YEAR))
      )

    #--- Apply control quality on extreme values
    out_cycl <- quantile(full_cov_raw$max_cyc, probs = 0.975)
    out_dhw  <- quantile(full_cov_raw$max_dhw, probs = 0.975)

    HexPred_sf <- full_cov_raw |>
      dplyr::mutate(As.Data = ifelse(Tier5 %in% data.grp.tier$Tier5, "Yes", "No")) |>
      dplyr::mutate(across(matches("^max_cyc.*"),
                           ~ ifelse(.x >= out_cycl & As.Data == "No", NA, .x))) |>
      dplyr::mutate(across(matches("^max_dhw.*"),
                           ~ ifelse(.x >= out_dhw & As.Data == "No", NA, .x)))

    #--- Select covariates
    selected_covar <- reefCloudPackage::select_covariates(HexPred_sf, i, N)

    ## Scale covariates
    HexPred_sf <- HexPred_sf |>
      dplyr::mutate(across(
       matches("^severity.*|^max.*"),
      ~ as.numeric((. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE))
     ))

    #--- Create reefid
    covs.hexpred_tier_sf_v2_prep <- reefCloudPackage::make_reefid(tier.sf.joined, HexPred_sf, reef_layer.sf, i, N) 

    #--- Merge reefid with covariates
    HexPred_reefid <- covs.hexpred_tier_sf_v2_prep |>
      dplyr::group_by(Tier5) |>
      dplyr::summarise(reefid = paste0(reefid, collapse = "_")) |>
      dplyr::ungroup()

    HexPred_reefid2 <- inner_join(HexPred_sf |> data.frame(), HexPred_reefid) |>
      dplyr::group_by(Tier5, fYEAR) |>
      dplyr::filter(row_number() == 1) |>
      dplyr::mutate(across(everything(), ~ replace(.x, is.na(.x), 0))) |>
      sf::st_as_sf(sf_column_name = "geometry")

    #--- Filter observations outside Tier5
    data.grp.tier.ready <- reefCloudPackage::rm_obs_outside(data.grp.tier, HexPred_reefid2, i, N)
   
    ## Log removed observations and stop if more than 30% of observations are outside tier5 cells 

    diff_perc <- ((nrow(data.grp.tier) - nrow(data.grp.tier.ready)) / nrow(data.grp.tier)) * 100
     if (diff_perc > 30) {
       msg <- paste(diff_perc, "% of data locations are outside Tier5 cells for", FOCAL_TIER, ":", TIER)
       status:::status_log("ERROR", log_file = log_file, "--Fitting INLA model--", msg = msg)
     next
    }

     ## Test if more than one reef in the final data 
    test_reefid <- HexPred_reefid2 %>% filter(Tier5 %in% data.grp.tier.ready$Tier5)

    #--- Prepare model objects
    obj_inla <- reefCloudPackage::inla_prep(data.grp.tier.ready, HexPred_reefid2, i, N)
    data.sub <- obj_inla$data.sub

    #--- Build formula
    if (length(selected_covar) == 0 && length(unique(test_reefid$reefid)) > 1) {
      formula_string <- paste(
        "COUNT ~ fYEAR +",
        "f(reefid, model = 'iid') +",
        "f(P_CODE, model = 'iid') +",
        "f(Site, model = 'iid') +",
        "f(Transect, model = 'iid') +",
        "f(fDEPTH, model = 'iid', hyper = list(prec = list(param = c(0.001, 0.001))))"
      )
    } else if (length(selected_covar) == 0 && length(unique(test_reefid$reefid)) == 1) {
      formula_string <- paste(
        "COUNT ~ fYEAR +",
        "f(P_CODE, model = 'iid') +",
        "f(Site, model = 'iid') +",
        "f(Transect, model = 'iid') +",
        "f(fDEPTH, model = 'iid', hyper = list(prec = list(param = c(0.001, 0.001))))"
      )
     } else if (length(selected_covar) != 0 && length(unique(test_reefid$reefid)) == 1) {
      formula_string <- paste(
        "COUNT ~ fYEAR +",
        paste(selected_covar, collapse = " + "), "+",
        "f(P_CODE, model = 'iid') +",
        "f(Site, model = 'iid') +",
        "f(Transect, model = 'iid') +",
        "f(fDEPTH, model = 'iid', hyper = list(prec = list(param = c(0.001, 0.001))))"
      )
    } else {
      formula_string <- paste(
        "COUNT ~ fYEAR +",
        paste(selected_covar, collapse = " + "), "+",
        "f(reefid, model = 'iid') +",
        "f(P_CODE, model = 'iid') +",
        "f(Site, model = 'iid') +",
        "f(Transect, model = 'iid') +",
        "f(fDEPTH, model = 'iid', hyper = list(prec = list(param = c(0.001, 0.001))))"
      )
    }

    model_formula_full <- as.formula(formula_string)
    model_formula <- reefCloudPackage::rm_factor(model_formula_full, data.sub)

    ## Test for rank deficiencies 
    # result_rank <- reefCloudPackage::rank_checks(data.grp.tier.ready, HexPred_reefid2, selected_covar)

    # if (result_rank$status == "fail"){
    #   # msg <- paste("Model is ranking deficient for", FOCAL_TIER, ":", TIER)
    #   # status:::status_log("ERROR", log_file = log_file, "--Fitting INLA model--", msg = msg )
    # next
    # }

    # ## Update formula 

    # model_formula <- as.formula(result_rank$formula)

    #--- Fit model
    status::status_try_catch(
     {
    M <- INLA::inla(
      model_formula,
      family = "binomial",
      Ntrials = TOTAL,
      data = data.sub,
      control.predictor = list(link = 1, compute = TRUE),
      control.compute = list(config = TRUE),
      silent = 2L
    )

    # Update status 
      old_item_name <- get_status_name(4, "INLA_fit")
        if (!str_detect(old_item_name, "\\[")) {
        new_item_name = paste(old_item_name,"[",i," / ", N,"]")
        } else{
        new_item_name <- str_replace(old_item_name, "\\[([^\\]]*)\\]", paste("[",i," / ", N,"]"))
        }
      status:::update_status_name(stage = 4, item = "INLA_fit", name = new_item_name)

     },
     stage_ = 4,
     order_ = 14,
     name_ = "Fit INLA model",
     item_ = "INLA_fit"
   )

    # Handle failed model
    if (length(M) == 0) {
       msg <- paste("Model failed to fit for", FOCAL_TIER, ":", TIER)
       status:::status_log("ERROR", log_file = log_file, "--Fitting INLA model--", msg = msg)
      next
    }

  #   ##############################
  #   #### Predict & summarise
  #   ##############################

  n_samples <- 1000
  samples <- INLA::inla.posterior.sample(n_samples, M)

  # 2. Identify the predictor indices
  predictor_idx <- grep("^Predictor", rownames(samples[[1]]$latent))

  # 3. Extract predictor samples
  latent_samples <- sapply(samples, function(x) x$latent[predictor_idx])

  #--- Predictions at data locations

  post_dist_df <- as.data.frame(latent_samples) |>
  dplyr::mutate(
    fYEAR = data.sub$fYEAR,
    Tier5 = data.sub$Tier5
  ) |>
  tidyr::pivot_longer(
    cols = -c(fYEAR, Tier5), 
    names_to = "draw",
    values_to = "pred"
  ) |>
  group_by(fYEAR, Tier5, draw) |>
  summarize(pred = mean(pred)) |>
  dplyr::mutate(
    id_loc = dplyr::row_number(),
    pred = plogis(pred),       
    model_name = "INLA"
  ) |>
  dplyr::select(fYEAR, Tier5, id_loc, draw, pred, model_name)
  
  #--- Summary predictions by Tier5
  tier.sf.joined$Tier5 <- as.factor(tier.sf.joined$Tier5)

    pred_sum_sf <- post_dist_df |>
      dplyr::group_by(fYEAR, Tier5) |>
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
    status::status_try_catch(
     {
    saveRDS(
      list(
        form = model_formula,
        data.grp.tier = data.sub,
        pred_sum_sf = pred_sum_sf,
        post_dist_df = post_dist_df,
        M = M
      ),
      file = paste0(DATA_PATH, "modelled/", "INLA_", FOCAL_TIER, "_", TIER, "_", GROUP, ".RData")
    )
      # Update status 
      old_item_name <- get_status_name(4, "INLA_saved")
        if (!str_detect(old_item_name, "\\[")) {
        new_item_name = paste(old_item_name,"[",i," / ", N,"]")
        } else{
        new_item_name <- str_replace(old_item_name, "\\[([^\\]]*)\\]", paste("[",i," / ", N,"]"))
        }
      status:::update_status_name(stage = 4, item = "INLA_saved", name = new_item_name)

     },
     stage_ = 4,
     order_ = 15,
     name_ = "Saved INLA outputs",
     item_ = "INLA_saved"
   )
   }  
}
