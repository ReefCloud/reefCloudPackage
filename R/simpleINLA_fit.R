
## simpleINLA_fit -----------------------------------------------------------------------

#' @title Function
#' @description Description
#' @param parameters description
#' @return returned arguments description
#' @examples examples
#' @export

simpleINLA_fit <- function(data.sub, GROUP, TIER, .data) {
  list2env(.data, envir = environment())
  ## ---- simpleINLAFit
  form <- Y ~ fYEAR +
    f(DATA_TYPE, model='iid') +
    f(P_CODE, model = 'iid') +
    ## f(fDEPTH, model='iid', hyper = list(theta = list(prior = 'pc.prec', param = c(1,0.01)))) +
    ## f(fDEPTH, model='iid', hyper = list(theta = list(prior = 'loggamma', param = c(1,0.000000001)))) +
    f(fDEPTH, model='iid', hyper = list(prec = list(param = c(0.001, 0.001)))) +
    f(Tier4, model='iid'
      ## ,hyper=prec.prior,
     ,group=nYEAR, ngroup=length(unique(data.pred$nYEAR)), control.group=list(model='ar1')
      ) +
    f(Tier5, model='iid'
      ## ,hyper=prec.prior
     ,group=nYEAR, ngroup=length(unique(data.pred$nYEAR)), control.group=list(model='ar1')
      ) +
    f(Site, model='iid'
      ## ,hyper=prec.prior
      ) +
    f(Transect, model='iid'
      ## ,hyper=prec.prior
      )
  ## Remove iid terms with less than 2 levels
  term.labels <- attr(terms(form), 'term.labels')
  vars <- all.vars(form)[-1]
  vars <- vars[!vars %in% c('nYEAR', 'data.pred')]
  ## vars <- vars[-grep('spatial|spde', vars)]
  for (v in vars) {
    ## if (data.pred %>% pull(!!sym(v)) %>% levels() %>% length() < 2) {
    if (data.sub %>% pull(!!sym(v)) %>% levels() %>% length() < 2) {
      ## fixed effects
      if (v %in% term.labels) {
        wch <- which(v == term.labels)
        if (length(wch)==0) next
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

  ## if there are no 'fixed' effects, then put the intercept back in
  if (all(str_detect(term.labels, "^f\\("))) form <- update(form, .~.+1)
  
  data.pred <- as.list(data.pred)
  if (LEGACY_DATA & !all(is.na(data.pred$COVER))) {
    data.pred$Y <- with(data.pred, cbind(COUNT,COVER))
    family <- c('binomial','beta')
  } else {
    form <- update(form, COUNT ~.)
    family <- 'binomial'
  }
  print(form)
  fit <- function(form, family, data.pred) {
    ## environment(form) <- .GlobalEnv
    mod <- inla(form,
                family=family,
                Ntrials=TOTAL,
                data=data.pred,
                control.predictor=list(link=1, compute=TRUE),
                control.compute=list(config=TRUE),
                silent = 2L
                )
    if(length(mod)==1)  {## if the model fails, try witout depth
      cat('Trying again!\n\n')
      form1 <- form
      term.labels <- attr(terms(form1), 'term.labels')
      form <- update(form, paste0(".~.-", term.labels[[2]]))
      mod <- fit(form,
                 family,
                 data.pred)
    }
    mod
  }
  mod <- fit(form, family, data.pred)
  ## check if the model successfully fitted
  save(mod, file=paste0(DATA_PATH,'modelled/simplemod_',DOMAIN_NAME, "_", GROUP, '_TIER', TIER, '.RData'))
  draws <- inla.posterior.sample(1000, result=mod, seed=123) %>% suppressWarnings()
  save(draws, file=paste0(DATA_PATH,'modelled/simpledraws_', DOMAIN_NAME, "_", GROUP, '_TIER', TIER, '.RData'))
  list(form = form,
       family = family,
       data.pred = data.pred
       )
  ## ----end
}