
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
meshINLA_cellmeans <- function(mesh, newdata.grid.list, spatial_and_stack.list, year = 'fixed') {
  list2env(newdata.grid.list, envir = environment())
  list2env(spatial_and_stack.list, envir = environment())
  load(file=paste0(DATA_PATH,'modelled/meshdraws_', DOMAIN_NAME, "_", GROUP, '_TIER', TIER, '.RData'))

  proj.grid <- inla.mesh.projector(mesh, loc=as.matrix(newdata.coords))
  cellmeans = sapply(draws, function(x) x[['latent']])
  i.mod <- lapply(c('APredictor','^Predictor','spatial.field','Site','Transect','fYEAR[0-9]*[:][0-9]*$','cYEAR','fDEPTH', 'P_CODE'),
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
    mutate(geometry = list(newdata.grid %>%
                           filter(!is.na(Longitude)) %>%
                           dplyr::select(Tier5, Longitude, Latitude) %>%
                           distinct())) %>%
    suppressMessages()
  ## add Year
  if (length(i.mod[[6]] > 0)) {  #if multiple fixed years were estimated
    cellmeans.full <- cellmeans.full %>%
      left_join(cellmeans[i.mod[[6]],] %>%
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
  } else if (length(i.mod[[7]] > 0)){ # if modelling year as a random walk
    cellmeans.full <- cellmeans.full %>%
      left_join(cellmeans[i.mod[[7]],] %>%
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
  cellmeans.full
  ## save the newdata posterior to the bucket
  ## save(cellmeans.full, file=paste0(AWS_PATH, "outputs/cellmeans.full_", DOMAIN_NAME, "_", GROUP, '_TIER', TIER, ".RData"))
}
