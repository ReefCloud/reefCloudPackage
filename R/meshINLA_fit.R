
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
meshINLA_fit <- function(form, spatial_and_stack.list) {
  list2env(spatial_and_stack.list, envir = environment())
  mod.inla <- inla(form,
                   data = inla.stack.data(stack.est),
                   family= 'binomial',
                   Ntrials=Total,
                   control.predictor = list(compute = TRUE,
                                            link = 1,
                                            A = inla.stack.A(stack.est)
                                            ),
                   control.compute = list(config = TRUE),
                   verbose = FALSE)

  save(mod.inla, file=paste0(DATA_PATH,'modelled/meshmod_', DOMAIN_NAME, "_", GROUP, "_TIER", TIER, '.RData'))
  draws <- inla.posterior.sample(1000, result=mod.inla, seed=123) %>%  suppressWarnings()
  save(draws, file=paste0(DATA_PATH,'modelled/meshdraws_', DOMAIN_NAME, "_", GROUP, '_TIER', TIER, '.RData'))
}
