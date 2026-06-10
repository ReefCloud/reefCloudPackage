
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
meshINLA_formula <- function(obs.grid.list, spatial_and_stack.list, year = 'fixed') {
  list2env(obs.grid.list, envir = environment())
  list2env(spatial_and_stack.list, envir = environment())
  data.grid <- data.grid1 %>% mutate(cYEAR = as.numeric(fYEAR))

  if (year=='fixed') {
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
  } else {
    form <- y ~ 0 +
      f(spatial.field,
        model = spde,
        group = spatial.field.group,
        control.group = list(model = "ar1")) +
      f(cYEAR, model = 'rw1') +
      f(P_CODE, model = 'iid') +
      f(fDEPTH, model = 'iid') +
      f(Site, model = 'iid') +
      f(Transect, model = 'iid')
  }
  ## Remove iid terms with less than 2 levels
  term.labels <- attr(terms(form), 'term.labels')
  vars <- all.vars(form)[-1]
  vars <- vars[-grep('spatial|spde', vars)]
  for (v in vars) {
    if ((data.grid %>% pull(!!sym(v)) %>% levels() %>% length() < 2) & (data.grid %>% pull(!!sym(v)) %>% class() != "numeric")) {
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
  if (year!='fixed') form <- update(form, .~.+Intercept)
  return(form)
}
