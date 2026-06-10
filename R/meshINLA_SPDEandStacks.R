

#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
meshINLA_SPDEandStacks <- function(mesh, full.grid.list, obs.grid.list, year = 'fixed') {
  list2env(full.grid.list, envir = environment())
  list2env(obs.grid.list, envir = environment())

  spde <- inla.spde2.matern(mesh,
                            alpha = 2)
  i.spatial <- inla.spde.make.index('spatial.field',
                                    n.spde = spde$n.spde,
                                    n.group = length(unique(full.grid$fYEAR))
                                    )
  data.grid2 <- data.grid %>% dplyr::select() %>% distinct() %>%
    st_centroid() %>%
    mutate(X=st_coordinates(.)[,1], Y=st_coordinates(.)[,2]) %>%
    st_drop_geometry() %>%
    crossing(fYEAR = full.grid %>% pull(fYEAR) %>% unique())

  data.grid1 <- data.grid %>%
    st_centroid() %>%
    mutate(X = st_coordinates(.)[,1], Y = st_coordinates(.)[,2]) %>%
    right_join(data.grid2)
  ## A.est <- inla.spde.make.A(mesh = mesh,
  ##                           loc = coords,
  ##                           group = as.numeric(data.grid$fYEAR)
  ##                           )
  A.est <- inla.spde.make.A(mesh = mesh,
                            loc = data.grid1 %>%
                            ## loc = data.grid %>%
                            ##   st_centroid() %>%
                            ##   st_coordinates() %>%
                              `[`(,c('X','Y')) %>% st_drop_geometry() %>% as.matrix(),
                            group = as.numeric(data.grid1$fYEAR)
                            )
  ## A.pred <- inla.spde.make.A(mesh = mesh,
  ##                            loc = as.matrix(full.grid %>% dplyr::select(Longitude,Latitude)),
  ##                            group = as.numeric(full.grid$fYEAR)
  ##                            )
  ## stack.est <- inla.stack(data = list(y = data.grid$COUNT,
  ##                                     Total=data.grid$TOTAL),
  ##                         A = list(A.est, list(1), list(1), list(1), list(1), list(1)),
  ##                         effects = list(
  ##                           i.spatial,
  ##                           list(fYEAR = data.grid$fYEAR),
  ##                           list(fDEPTH = data.grid$fDEPTH),
  ##                           list(P_CODE = data.grid$P_CODE),
  ##                           list(Site = data.grid$Site),
  ##                           list(Transect = data.grid$Transect)
  ##                         ),
  ##                         tag = 'est'
  ##                         )
  if (year == 'fixed') {
    stack.est <- inla.stack(data = list(y = data.grid1$COUNT,
                                        Total=data.grid1$TOTAL),
                            A = list(A.est, list(1), list(1), list(1), list(1), list(1)),
                            effects = list(
                              i.spatial,
                              list(fYEAR = data.grid1$fYEAR),
                              list(fDEPTH = data.grid1$fDEPTH),
                              list(P_CODE = data.grid1$P_CODE),
                              list(Site = data.grid1$Site),
                              list(Transect = data.grid1$Transect)
                            ),
                            tag = 'est'
                            )
  } else {
    stack.est <- inla.stack(data = list(y = data.grid1$COUNT,
                                        Total=data.grid1$TOTAL),
                            A = list(A.est, list(1), list(1), list(1), list(1), list(1), list(1), list(1)),
                            effects = list(
                              i.spatial,
                              list(Intercept = rep(1:nrow(data.grid1))),
                              list(fYEAR = data.grid1$fYEAR),
                              list(cYEAR = as.numeric(as.character(data.grid1$fYEAR))),
                              list(fDEPTH = data.grid1$fDEPTH),
                              list(P_CODE = data.grid1$P_CODE),
                              list(Site = data.grid1$Site),
                              list(Transect = data.grid1$Transect)
                            ),
                            tag = 'est'
                            )

  }
  ## We could pre-define a prediction stack, however, the model will be substantially slower
  ## stack.pred <- inla.stack(data = list(y = NA,
  ##                                      TOTAL=1),
  ##                          A = list(A.pred, list(1), list(1), list(1), list(1), list(1)),
  ##                          effects = list(
  ##                            i.spatial,
  ##                            list(fYEAR = full.grid$fYEAR),
  ##                            list(fDEPTH = rep(NA, nrow(full.grid))),
  ##                            list(P_CODE = rep(NA, nrow(full.grid))),
  ##                            list(Site = rep(NA,nrow(full.grid))),
  ##                            list(Transect = rep(NA, nrow(full.grid)))
  ##                          ),
  ##                          tag = 'pred'
  ##                          )
  ## stack.all <- inla.stack(stack.est, stack.pred)
  list(spde = spde,
       i.spatial = i.spatial,
       ## A.est = A.est,
       stack.est = stack.est,
       data.grid1 = data.grid1)
}
