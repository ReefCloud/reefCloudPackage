
extract_reef_id_tier <- function(full_cov, tier.sf) {
  load(file = paste0(DATA_PATH, "/primary/reef_layer.sf.RData"))
  full_cov.sf <-
    full_cov %>% 
    left_join(tier.sf) %>%
    st_as_sf() %>%
    st_transform(crs = st_crs(reef_layer.sf)) %>% 
    suppressMessages()%>%
    suppressWarnings()

  ## Crop Reef layer and create reefid
  ##Make sure that st_crs(covs.hexpred_tier_sf)$units
  ##and st_crs(reef_layer.sf)$units are metres
  reef_layer_crop.sf <- reef_layer.sf %>% 
    st_crop(full_cov.sf) %>%
    st_cast("POLYGON") %>%
    mutate(reefid = row_number()) %>%
    dplyr::select(-GRIDCODE)%>%
    st_transform(crs = 4326) %>%
    st_buffer(dist=450) %>%#careful with the units and version of sf
    suppressMessages()%>%
    suppressWarnings()

  full_cov.sf <- full_cov.sf %>%
    st_transform(crs = 4326)

  ## Join the two shapefiles
  ## sf_use_s2(TRUE)
  sf_use_s2(FALSE)
  covs.hexpred_tier_sf_v2_prep <- full_cov.sf %>%
    st_join(reef_layer_crop.sf) %>%
    dplyr::select(Tier5, reefid, geometry)%>%
    suppressMessages()%>%
    suppressWarnings()

  ## Check number of Tier5 without reefid
  missing_reefid <- covs.hexpred_tier_sf_v2_prep %>%
    st_drop_geometry() %>%
    map_df(~sum(is.na(.)))

  if (missing_reefid$reefid[1] > 0) {
    msg <- (paste("Some Tier5 do not have a reefid for the",FOCAL_TIER,":", TIER))
    reefCloudPackage::log("WARNING", logFile = LOG_FILE,
      "--Fitting model 5 FRK--", msg = msg)

    covs.hexpred_tier_sf_v2_prep <- covs.hexpred_tier_sf_v2_prep[-which(is.na(covs.hexpred_tier_sf_v2_prep$reefid)),]
  }
  covs.hexpred_tier_sf_v2_prep
}
