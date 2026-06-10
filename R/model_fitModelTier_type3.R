#' @title Fit model at tier level
#' @description Fits model to data at tier level
#' @param data.grp data on which model is fitted
#' @examples model_fitModelTier()
#' @export
model_fitModelTier_type3 <- function(data.grp){

    if (reefCloudPackage::isParent()) reefCloudPackage::startMatter()

    ## reefCloudPackage::ReefCloud_tryCatch({
      tiers.lookup <<- reefCloudPackage::get_tiers_lookup()
      tier5.sf <- reefCloudPackage::get_tier_data(5)

      ## reefCloudPackage::ReefCloud_tryCatch({
        TIERS <- NULL
        for (TIER in unique(data.grp[[FOCAL_TIER]])) {
          TIER <<- TIER  #make this global
          ## cli::cli_progress_bar("SPDE INLA for {stringr::str_to_title(GROUP)}, Tier {TIER} cellmeans", type = "iterator", total = 19, clear = TRUE)
          ## Subset the FOCAL TIER with a buffer
          tier5_buffer.list <- reefCloudPackage::meshINLA_tier_sub(FOCAL_TIER, TIER, tier5.sf)
          ## Subset the data
          data.sub <- reefCloudPackage::meshINLA_data_grid(data.grp, tier5.sf, tier5_buffer.list)
          if(nrow(data.sub) >1) {
            TIERS <- c(TIERS, TIER)
            reefCloudPackage::meshINLA(data.sub, tier5.sf, tier5_buffer.list, FOCAL_TIER)
          } else {
            cli_alert_warning("Modelling for {stringr::str_to_title(GROUP)}, Tier {TIER} not performed due to a lack of data")
            reefCloudPackage::log('WARNING', logFile = LOG_FILE,
                          Category = '--Modelling (mesh INLA)--',
                          msg = paste0(DOMAIN_NAME, ": ", stringr::str_to_title(GROUP),"/Tier:", TIER, "/Mesh INLA cellmeans not calculated - not enough data"))
          }
        }
        ## Gather the Tier5 posteriors
        cellmeans.full <- reefCloudPackage::meshINLA_reconstruct(GROUP, TIERS)

        cellmeans.full.tier4 <- reefCloudPackage::meshINLA_tier4(cellmeans.full)
        reefCloudPackage::meshINLA_tier4TemporalFocal(cellmeans.full.tier4)
        reefCloudPackage::meshINLA_tier4Spatiotemporal(cellmeans.full.tier4)

        cellmeans.full.tier3 <- reefCloudPackage::meshINLA_tier3(cellmeans.full)
        reefCloudPackage::meshINLA_tier3TemporalFocal(cellmeans.full.tier3)
        reefCloudPackage::meshINLA_tier3Spatiotemporal(cellmeans.full.tier3)

        cellmeans.full.tier2 <- reefCloudPackage::meshINLA_tier2(cellmeans.full)
        reefCloudPackage::meshINLA_tier2TemporalFocal(cellmeans.full.tier2)
        reefCloudPackage::meshINLA_tier2Spatiotemporal(cellmeans.full.tier2)

      ## }, logFile=LOG_FILE, Category='--Modelling fitting routines--',
      ## msg=paste0('Mesh INLA cell means for ', stringr::str_to_title(GROUP)), return=NULL)

      if (1==2) {
        ## reefCloudPackage::ReefCloud_tryCatch({
          ## ---- MeshINLA
          {
            cli::cli_progress_bar("SPDE INLA cellmeans", type = "iterator", total = 19, clear = TRUE)
            ## ---- meshINLADataGrid
            tier5.sf <- get(load(paste0(DATA_PATH, 'primary/tier5.sf.RData')))
            full.grid <- tier5.sf %>%
              st_centroid() %>%
              mutate(Longitude = st_coordinates(.)[,1],
                     Latitude = st_coordinates(.)[,2]) %>%
              st_drop_geometry() %>%
              crossing(
                P_CODE = unique(data.sub$P_CODE),
                fYEAR = unique(data.sub$fYEAR),
                fDEPTH = unique(data.sub$fDEPTH)) %>%
              mutate(fYEAR = factor(fYEAR, levels = rev(sort(levels(fYEAR))))) %>%
              suppressWarnings()

            full.coords <- full.grid %>%
              dplyr::select(Longitude,Latitude) %>%
              distinct()
            cli::cli_progress_update(force=TRUE)
            ## ----end
            ## ---- meshINLADataObs
            data.grid <- data.sub %>%
              left_join(tier5.sf) %>%
              mutate(
                P_CODE = factor(P_CODE),
                Site = factor(interaction(Tier5, Site)),
                Transect = factor(interaction(Site, Transect))) %>%
              dplyr::select(P_CODE, fYEAR, fDEPTH, Site, Transect, COUNT, TOTAL, geometry) %>%
              distinct() %>%
              st_as_sf() %>%
              mutate(fYEAR = factor(fYEAR, levels = rev(sort(levels(fYEAR))))) %>%
              suppressMessages()

            coords <- data.grid %>%
              st_centroid() %>%
              st_coordinates() %>%
              `[`(,c('X','Y')) %>%
              suppressWarnings()
            cli::cli_progress_update(force=TRUE)
            ## ----end
            ## ---- meshINLADataMesh
            tier4.sf <- get(load(paste0(DATA_PATH, 'primary/tier4.sf.RData')))
            bndry <- tier4.sf %>%
              st_make_valid() %>%
              st_buffer(0.1) %>%
              st_union() %>%
              ## st_cast("GEOMETRY")
              st_coordinates() %>%
              `[`(,c('X','Y')) %>%
              suppressMessages() %>%
              suppressWarnings()

            mesh <- inla.mesh.2d(loc = full.coords,
                                 boundary = bndry,
                                 max.edge = c(500, 500),
                                 cutoff = 0.01
            )

            g1 <-
              ggplot() +
              inlabru::gg(mesh) +
              geom_sf(data=data.grid) +
              theme_bw() +
              theme(axis.title = element_blank())

            ggsave(filename=paste0(OUTPUT_PATH, 'figures/mesh_', DOMAIN_NAME, "_", str_remove_all(GROUP,' '), '.png'), g1, width=5, height=6, dpi=72)
            cli::cli_progress_update(force=TRUE)
            ## ----end
            ## ---- meshINLASPDEandStacks
            spde <- inla.spde2.matern(mesh,
                                      alpha = 2)
            i.spatial <- inla.spde.make.index('spatial.field',
                                              n.spde = spde$n.spde,
                                              n.group = length(unique(full.grid$fYEAR))
            )
            A.est <- inla.spde.make.A(mesh = mesh,
                                      loc = coords,
                                      group = as.numeric(data.grid$fYEAR)
            )
            A.pred <- inla.spde.make.A(mesh = mesh,
                                       loc = as.matrix(full.grid %>% dplyr::select(Longitude,Latitude)),
                                       group = as.numeric(full.grid$fYEAR)
            )
            stack.est <- inla.stack(data = list(y = data.grid$COUNT,
                                                Total=data.grid$TOTAL),
                                    A = list(A.est, list(1), list(1), list(1), list(1), list(1)),
                                    effects = list(
                                      i.spatial,
                                      list(fYEAR = data.grid$fYEAR),
                                      list(fDEPTH = data.grid$fDEPTH),
                                      list(P_CODE = data.grid$P_CODE),
                                      list(Site = data.grid$Site),
                                      list(Transect = data.grid$Transect)
                                    ),
                                    tag = 'est'
            )
            ## We could pre-define a prediction stack, however, the model will be substantially slower
            stack.pred <- inla.stack(data = list(y = NA,
                                                 TOTAL=1),
                                     A = list(A.pred, list(1), list(1), list(1), list(1), list(1)),
                                     effects = list(
                                       i.spatial,
                                       list(fYEAR = full.grid$fYEAR),
                                       list(fDEPTH = rep(NA, nrow(full.grid))),
                                       list(P_CODE = rep(NA, nrow(full.grid))),
                                       list(Site = rep(NA,nrow(full.grid))),
                                       list(Transect = rep(NA, nrow(full.grid)))
                                     ),
                                     tag = 'pred'
            )
            stack.all <- inla.stack(stack.est, stack.pred)

            cli::cli_progress_update(force=TRUE)
            ## ----end
            ## ---- meshINLAFormula
            form <- y ~ 0 +
              fYEAR +
              f(spatial.field,
                model = spde,
                group = spatial.field.group,
                control.group = list(model = "ar1")) +
              f(P_CODE, model = 'iid') +
              f(fDEPTH, model = 'iid') +
              f(Site, model = 'iid') +
              f(Transect, model = 'iid')
            ## Remove iid terms with less than 2 levels
            term.labels <- attr(terms(form), 'term.labels')
            vars <- all.vars(form)[-1]
            vars <- vars[-grep('spatial|spde', vars)]
            for (v in vars) {
              if ((points.d %>% pull(!!sym(v)) %>% levels() %>% length() < 2) & (points.d %>% pull(!!sym(v)) %>% class() != "numeric")) {
                ## fixed effects
                if (v %in% term.labels) {
                  wch <- which(v == term.labels)
                  if (length(wch) ==0) next
                  term.labels <- term.labels[-wch]
                  form <- reformulate(term.labels, response = all.vars(form)[1], intercept = FALSE)
                }
                ## random effects
                wch <- grep(paste0('^f.',v), term.labels, perl = TRUE)
                if (length(wch)==0) next
                term.labels <- term.labels[-wch]
                form <- reformulate(term.labels, response = all.vars(form)[1], intercept = FALSE)
              }
            }
            cli::cli_progress_update(force=TRUE)
            ## ----end
            ## ---- meshINLAFit
            mod.inla <- inla(form,
                             data = inla.stack.data(stack.est),
                             ## data = inla.stack.data(stack.all),
                             family= 'binomial',
                             Ntrials=Total,
                             control.predictor = list(compute = TRUE,
                                                      link = 1,
                                                      A = inla.stack.A(stack.est)
                                                      ## A = inla.stack.A(stack.all)
                             ),
                             control.compute = list(config = TRUE),
                             verbose = FALSE)

            save(mod.inla, file=paste0(DATA_PATH,'modelled/meshmod_', DOMAIN_NAME, "_", GROUP, '.RData'))
            draws <- inla.posterior.sample(1000, result=mod.inla, seed=123) %>%  suppressWarnings()
            save(draws, file=paste0(DATA_PATH,'modelled/meshdraws_', DOMAIN_NAME, "_", GROUP, '.RData'))
            cli::cli_progress_update(force=TRUE)
            ## ----end
            ## ---- meshINLAFullPosteriors
            proj.grid <- inla.mesh.projector(mesh, loc=as.matrix(full.coords))
            cellmeans = sapply(draws, function(x) x[['latent']])
            i.mod <- lapply(c('APredictor','^Predictor','spatial.field','Site','Transect','fYEAR[0-9]*[:][0-9]*$','fDEPTH', 'P_CODE'),
                            function(x) grep(x, draws[[1]]$latent %>% rownames))

            cellmeans.full <- cellmeans[i.mod[[3]],] %>%          ## retrieve the spatial.fields posteriors
              as.data.frame %>%                                 ## convert to data.frame
              mutate(fYEAR = rep(as.numeric(levels(stack.est$effects$data$fYEAR)),
                                 each = which(i.spatial$spatial.field.group == 1) %>% length)) %>%
              group_by(fYEAR) %>%
              nest() %>%
              ## project onto spatial field
              mutate(Spatial = map(.x = data,
                                   .f = function(x)
                                     as.matrix(inla.mesh.project(proj.grid, x)))) %>%
              mutate(geometry = list(full.grid %>%
                                       dplyr::select(Tier5, Longitude, Latitude) %>%
                                       distinct())) %>%
              suppressMessages()
            ## add Year
            if (length(i.mod[[6]] > 0)) {  #if multiple years were estimated
              cellmeans.full <- cellmeans.full %>%
                full_join(cellmeans[i.mod[[6]],] %>%
                            as.data.frame() %>%
                            mutate(fYEAR = as.numeric(levels(stack.est$effects$data$fYEAR))) %>%
                            group_by(fYEAR) %>%
                            nest() %>%
                            rename(data1 = data)) %>%
                mutate(value = map2(.x = Spatial, .y = data1,
                                    .f = function(.x, .y) as.data.frame(sweep(.x, 2, unlist(.y), FUN = "+")))) %>%
                dplyr::select(fYEAR, geometry, value) %>%
                unnest(cols = c(geometry, value)) %>%
                pivot_longer(cols = starts_with("V"), names_to = "Rep") %>%
                ungroup() %>%
                suppressMessages()
            } else { # if there was only a single year
              cellmeans.full <- cellmeans.full %>%
                mutate(fYEAR = as.numeric(levels(stack.est$effects$data$fYEAR))) %>%
                mutate(value = map(.x = Spatial,
                                   .f = function(.x) as.data.frame(.x))) %>%
                dplyr::select(fYEAR, geometry, value) %>%
                unnest(cols = c(geometry, value)) %>%
                pivot_longer(cols = starts_with("V"), names_to = "Rep") %>%
                ungroup() %>%
                suppressMessages()

            }
            ## save the full posterior to the bucket
            save(cellmeans.full, file=paste0(AWS_PATH, "outputs/cellmeans.full_", DOMAIN_NAME, "_", GROUP, ".RData"))
            cli::cli_progress_update(force=TRUE)
            ## ----end
            ## ---- meshINLATier5TemporalFocal
            tiers.lookup <- reefCloudPackage::get_tiers_lookup()
            ## Restrict this to just the observed (focal)Tier5 hexagons, otherwise the grid of plots will be huge!
            cellmeans.tier5 <- cellmeans.full %>%
              right_join(data.sub %>% dplyr::select(Tier5, Tier4) %>% distinct()) %>%
              group_by(fYEAR, Tier4, Tier5) %>%
              mutate(Latitude = mean(Latitude)) %>%
              ungroup() %>%
              mutate(Tier4 = forcats::fct_reorder(Tier4, Latitude, min),
                     Tier5 = forcats::fct_reorder(Tier5, Latitude, min)) %>%
              group_by(fYEAR, Tier4, Tier5) %>%
              mutate(value = plogis(value)) %>%
              reefCloudPackage::mean_median_hdci(value) %>%
              suppressMessages()
            saveRDS(list(cellmeans.tier5=cellmeans.tier5, data = data.sub),
                    file = paste0(DATA_PATH, "modelled/", "cellmeans_meshTemporalTier5focal_", DOMAIN_NAME, "_", GROUP, ".RData"))
            cli::cli_progress_update(force=TRUE)
            ## ----end
            ## ---- meshINLATier5Temporal
            tiers.lookup <- reefCloudPackage::get_tiers_lookup()
            cellmeans.tier5 <- cellmeans.full %>%
              left_join(tiers.lookup %>% dplyr::select(Tier4, Tier5) %>% distinct()) %>%
              group_by(fYEAR, Tier4, Tier5) %>%
              mutate(Latitude = mean(Latitude)) %>%
              ungroup() %>%
              mutate(Tier4 = forcats::fct_reorder(Tier4, Latitude, min),
                     Tier5 = forcats::fct_reorder(Tier5, Latitude, min)) %>%
              group_by(fYEAR, Tier4, Tier5) %>%
              mutate(value = plogis(value)) %>%
              reefCloudPackage::mean_median_hdci(value) %>%      #this step is slow?
              suppressMessages()
            saveRDS(list(cellmeans.tier5=cellmeans.tier5, data = data.sub),
                    file = paste0(DATA_PATH, "modelled/", "cellmeans_meshTemporalTier5_", DOMAIN_NAME, "_", GROUP, ".RData"))
            cellmeans.tier5.sum <- cellmeans.tier5
            save(cellmeans.tier5.sum, file = paste0(DATA_PATH, "summarised/cellmeans_INLASPDE_", DOMAIN_NAME, "_", GROUP, "_Tier5.RData"))
            cli::cli_progress_update(force=TRUE)
            ## ----end
            ## ---- meshINLATier5Spatiotemporal
            cellmeans.tier5.sf <- cellmeans.full %>%
              group_by(fYEAR, Tier5) %>%
              mutate(value = plogis(value)) %>%
              reefCloudPackage::mean_median_hdci(value) %>%
              left_join(tier5.sf) %>%
              st_as_sf() %>%
              ungroup() %>%
              suppressMessages()
            saveRDS(list(cellmeans.tier5.sf = cellmeans.tier5.sf, tier5.sf = tier5.sf, tier4.sf = tier4.sf),
                    file = paste0(DATA_PATH, "modelled/", "cellmeans_meshSpatioTemporalTier5_", DOMAIN_NAME, "_", GROUP, ".RData"))
            cli::cli_progress_update(force=TRUE)
            ## ----end
            ## ---- meshINLATier4Posteriors
            tiers.lookup <- reefCloudPackage::get_tiers_lookup()
            cellmeans.full.tier4 <- cellmeans.full %>%
              left_join(tiers.lookup) %>%
              group_by(Tier2, Tier3, Tier4, fYEAR, Rep) %>%
              mutate(wt = reef_area / sum(reef_area)) %>%
              summarise(value = sum(value * wt)) %>%
              suppressMessages()
            cli::cli_progress_update(force=TRUE)
            ## ----end
            ## ---- meshINLATier4Temporal
            cellmeans.tier4.sum <-
              cellmeans.full.tier4 %>%
              left_join(tier4.sf %>%
                          st_centroid() %>%
                          mutate(Latitude = st_coordinates(.) %>% `[[`(1)) %>%
                          dplyr::select(Tier4, Latitude) %>%
                          distinct()) %>%
              group_by(fYEAR, Tier3, Tier4) %>%
              mutate(Latitude = mean(Latitude)) %>%
              ungroup() %>%
              mutate(Tier3 = forcats::fct_reorder(Tier3, Latitude, min),
                     Tier4 = forcats::fct_reorder(Tier4, Latitude, min)) %>%
              group_by(fYEAR, Tier4) %>%
              mutate(value = plogis(value)) %>%
              reefCloudPackage::mean_median_hdci(value) %>%
              suppressMessages() %>%
              suppressWarnings()
            saveRDS(list(cellmeans.tier4.sum=cellmeans.tier4.sum, data = data.sub),
                    file = paste0(DATA_PATH, "modelled/", "cellmeans_meshTemporalTier4_", DOMAIN_NAME, "_", GROUP, ".RData"))
            save(cellmeans.tier4.sum, file = paste0(DATA_PATH, "summarised/cellmeans_INLASPDE_", DOMAIN_NAME, "_", GROUP, "_Tier4.RData"))
            cli::cli_progress_update(force=TRUE)
            ## ----end
            ## ---- meshINLATier4Spatiotemporal
            cellmeans.tier4.sf <- cellmeans.full.tier4 %>%
              group_by(fYEAR, Tier4) %>%
              mutate(value = plogis(value)) %>%
              reefCloudPackage::mean_median_hdci(value) %>%
              left_join(tier4.sf) %>%
              st_as_sf() %>%
              ungroup() %>%
              suppressMessages()
            saveRDS(list(cellmeans.tier4.sf = cellmeans.tier4.sf, tier4.sf = tier4.sf),
                    file = paste0(DATA_PATH, "modelled/", "cellmeans_meshSpatioTemporalTier4_", DOMAIN_NAME, "_", GROUP, ".RData"))
            cli::cli_progress_update(force=TRUE)
            ## ----end
            ## ---- meshINLATier3Posteriors
            tiers.lookup <- reefCloudPackage::get_tiers_lookup()
            cellmeans.full.tier3 <- cellmeans.full.tier4 %>%
              left_join(tiers.lookup) %>%
              group_by(Tier2, Tier3, fYEAR, Rep) %>%
              mutate(wt = reef_area / sum(reef_area)) %>%
              summarise(value = sum(value * wt)) %>%
              suppressMessages()
            cli::cli_progress_update(force=TRUE)
            ## ----end
            ## ---- meshINLATier3Temporal
            tier3.sf <- get(load(paste0(DATA_PATH,'primary/tier3.sf.RData')))
            cellmeans.tier3.sum <-
              cellmeans.full.tier3 %>%
              left_join(tier3.sf %>%
                          st_centroid() %>%
                          mutate(Latitude = st_coordinates(.) %>% `[[`(1)) %>%
                          dplyr::select(Tier3, Latitude) %>%
                          distinct()) %>%
              group_by(fYEAR, Tier3) %>%
              mutate(Latitude = mean(Latitude)) %>%
              ungroup() %>%
              mutate(Tier3 = forcats::fct_reorder(Tier3, Latitude, min),
                     Tier3 = forcats::fct_reorder(Tier3, Latitude, min)) %>%
              group_by(fYEAR, Tier3) %>%
              mutate(value = plogis(value)) %>%
              reefCloudPackage::mean_median_hdci(value) %>%
              suppressMessages() %>%
              suppressWarnings()
            saveRDS(list(cellmeans.tier3.sum=cellmeans.tier3.sum, data = data.sub),
                    file = paste0(DATA_PATH, "modelled/", "cellmeans_meshTemporalTier3_", DOMAIN_NAME, "_", GROUP, ".RData"))
            save(cellmeans.tier3.sum, file = paste0(DATA_PATH, "summarised/cellmeans_INLASPDE_", DOMAIN_NAME, "_", GROUP, "_Tier3.RData"))
            cli::cli_progress_update(force=TRUE)
            ## ----end
            ## ---- meshINLATier3Spatiotemporal
            cellmeans.tier3.sf <- cellmeans.full.tier3 %>%
              group_by(fYEAR, Tier3) %>%
              mutate(value = plogis(value)) %>%
              reefCloudPackage::mean_median_hdci(value) %>%
              left_join(tier3.sf) %>%
              st_as_sf() %>%
              ungroup() %>%
              suppressMessages()
            saveRDS(list(cellmeans.tier3.sf = cellmeans.tier3.sf, tier3.sf = tier3.sf),
                    file = paste0(DATA_PATH, "modelled/", "cellmeans_meshSpatioTemporalTier3_", DOMAIN_NAME, "_", GROUP, ".RData"))
            cli::cli_progress_update(force=TRUE)
            ## ----end
            ## ---- meshINLATier2Posteriors
            tiers.lookup <- reefCloudPackage::get_tiers_lookup()
            cellmeans.full.tier2 <- cellmeans.full.tier3 %>%
              left_join(tiers.lookup) %>%
              group_by(Tier2, fYEAR, Rep) %>%
              mutate(wt = reef_area / sum(reef_area)) %>%
              summarise(value = sum(value * wt)) %>%
              suppressMessages()
            cli::cli_progress_update(force=TRUE)
            ## ----end
            ## ---- meshINLATier2Temporal
            tier2.sf <- get(load(paste0(DATA_PATH,'primary/tier2.sf.RData')))
            cellmeans.tier2.sum <-
              cellmeans.full.tier2 %>%
              left_join(tier2.sf %>%
                          st_centroid() %>%
                          mutate(Latitude = st_coordinates(.) %>% `[[`(1)) %>%
                          dplyr::select(Tier2, Latitude) %>%
                          distinct()) %>%
              group_by(fYEAR, Tier2) %>%
              mutate(Latitude = mean(Latitude)) %>%
              ungroup() %>%
              mutate(Tier2 = forcats::fct_reorder(Tier2, Latitude, min),
                     Tier2 = forcats::fct_reorder(Tier2, Latitude, min)) %>%
              group_by(fYEAR, Tier2) %>%
              mutate(value = plogis(value)) %>%
              reefCloudPackage::mean_median_hdci(value) %>%
              suppressMessages() %>%
              suppressWarnings()
            saveRDS(list(cellmeans.tier2.sum=cellmeans.tier2.sum, data = data.sub),
                    file = paste0(DATA_PATH, "modelled/", "cellmeans_meshTemporalTier2_", DOMAIN_NAME, "_", GROUP, ".RData"))
            save(cellmeans.tier2.sum, file = paste0(DATA_PATH, "summarised/cellmeans_INLASPDE_", DOMAIN_NAME, "_", GROUP, "_Tier2.RData"))
            cli::cli_progress_update(force=TRUE)
            ## ----end
            ## ---- meshINLATier2Spatiotemporal
            cellmeans.tier2.sf <- cellmeans.full.tier2 %>%
              group_by(fYEAR, Tier2) %>%
              mutate(value = plogis(value)) %>%
              reefCloudPackage::mean_median_hdci(value) %>%
              left_join(tier2.sf) %>%
              st_as_sf() %>%
              ungroup() %>%
              suppressMessages()
            saveRDS(list(cellmeans.tier2.sf = cellmeans.tier2.sf, tier2.sf = tier2.sf),
                    file = paste0(DATA_PATH, "modelled/", "cellmeans_meshSpatioTemporalTier2_", DOMAIN_NAME, "_", GROUP, ".RData"))
            ## ----end
            rm(cellmeans,
               #cellmeans.full,
               tier4.sf, tier5.sf, tier.sf, tier2.sf, tier3.sf,
               cellmeans.tier5.sum, cellmeans.tier5.sf, celmeans.tier5,
               cellmeans.tier4.sum, cellmeans.tier4.sf, cellmeans.full.tier4,
               cellmeans.tier3.sum, cellmeans.tier3.sf, cellmeans.full.tier3,
               cellmeans.tier2.sum, cellmeans.tier2.sf, cellmeans.full.tier2,
               stack.all, stack.est, stack.pred, spde, mesh, mod, mod.inla, draws,
               proj,gird, prec.prior, i.mod, i.spatial, form, fit, full.coords, full.grid,
               data.pred, data.tmp, data.site, data.grid, data.sum
            ) %>% suppressWarnings()
            invisible(gc(full=TRUE))
            cli::cli_progress_update(force=TRUE)
          }
          cli::cli_progress_done()
          ## ----end
          cat(paste0('Modelling complete:\n'))
      ##   }, logFile=LOG_FILE, Category='--Modelling fitting routines--',
      ##   msg=paste0('Mesh INLA cell means for ', stringr::str_to_title(GROUP)), return=NULL)
      }
    ## },
    ## logFile=LOG_FILE,
    ## Category='--Modelling fitting routines--',
    ## msg='Fit simple model (Type 3)',
    ## return=NULL,
    ## stage = paste0("STAGE", CURRENT_STAGE),
    ## item = "model_type3"
    ## )

}
