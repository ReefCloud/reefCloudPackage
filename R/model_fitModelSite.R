#' @title Fit model at site level
#' @description Fits model to data at site level, temporal model
#' @param raw_cell_means boolean. If TRUE, runs raw cell means. If FALSE, runs simple INLA cell means
#' @examples model_fitModelSite()
#' @export
model_fitModelSite <- function(raw_cell_means=TRUE){

  if (reefCloudPackage::isParent()) reefCloudPackage::startMatter()

  ## load in the data
  ## reefCloudPackage::ReefCloud_tryCatch({
    load(file=paste0(DATA_PATH,'processed/',RDATA_FILE))
    SITES <- data %>% pull(SITE) %>% unique()
    data <- data %>%
      mutate(SITE_ZONE_DEPTH = interaction(SITE, ZONE_DEPTH, sep=';'))

    SITE_ZONE_DEPTHS <- data %>% pull(SITE_ZONE_DEPTH) %>% unique()
  ## }, logFile=LOG_FILE, Category='--Modelling fitting routines--', msg='Load data for modelling', return=NULL)

  for (iSITE_ZONE_DEPTH in SITE_ZONE_DEPTHS) {
    nSITE <- gsub('([^;]*);.*','\\1',iSITE_ZONE_DEPTH, perl=TRUE)
    nREEF_ZONE <- gsub('[^;]*;([^;]*);.*','\\1',iSITE_ZONE_DEPTH, perl=TRUE)
    nDEPTH <- gsub('[^;]*;[^;]*;(.*)','\\1',iSITE_ZONE_DEPTH, perl=TRUE)

    ## cli_alert("Modelling for {stringr::str_to_title(nSITE)}, at {nDEPTH}m depth in the {nREEF_ZONE} zone")  # Disabled - too verbose

    ## reefCloudPackage::ReefCloud_tryCatch({
      ## ---- data_sub.sites
      data.sub <- data %>%
        ## filter(SITE == iSITE) %>%
        filter(SITE_ZONE_DEPTH == iSITE_ZONE_DEPTH) %>%
        droplevels() %>%
        mutate(
          fGROUP = factor(fGROUP),
          P_CODE = factor(P_CODE),
          Site = factor(paste(P_CODE, SITE_NO)),
          Transect = factor(paste(Site, TRANSECT_NO))) %>%
        arrange(P_CODE, Site, Transect, desc(as.numeric(as.character(fYEAR)))) %>%
        mutate(fYEAR = factor(fYEAR, levels=unique(fYEAR))) %>%
        mutate(TOTAL = ifelse(is.infinite(TOTAL), 0, TOTAL))
      ## Tests -----------------
      ## data.sub %>% filter(DATA_TYPE == 'Data', SITE == 'Angaur', fGROUP == 'CRUSTOSE CORALLINE ALGAE') %>% pull(REPORT_YEAR) %>% unique
      ## data.sub %>% filter(DATA_TYPE == 'Legacy', SITE == 'Angaur', fGROUP == 'CRUSTOSE CORALLINE ALGAE') %>% pull(REPORT_YEAR) %>% unique
      ## -----------------------
      ## ----end
    ## }, logFile=LOG_FILE, Category='--Modelling fitting routines--',
    ## msg=paste0('Prepare ', stringr::str_to_title(nSITE),'/', nDEPTH, 'm depth/',nREEF_ZONE,' zone sub data for modelling'), return=NULL)


    if (raw_cell_means){
      ## reefCloudPackage::ReefCloud_tryCatch({
        ## ---- RawMeans.sites
        {
          cli::cli_progress_bar("Raw cellmeans", type = "iterator", total = 1, clear = TRUE)
          reefCloudPackage::log('INFO', logFile = LOG_FILE,
                        Category = '--Modelling (Raw means)--',
                        msg = paste0(DOMAIN_NAME, ": ", stringr::str_to_title(nSITE),"/",
                                     nDEPTH, "m depth/",nREEF_ZONE," zone Raw cellmeans calculated"))

          ##Model separately for:
          ## - fDEPTH
          ## - REEF_ZONE
          ##Model includes:
          ## - fYEAR
          ## - fGROUP

          ## ---- RawMeansTemporal.sites
          cellmeans <-
            data.sub %>%
            mutate(PERC_COVER = ifelse(!is.na(PERC_COVER), PERC_COVER, COVER)) %>%  # a single cover value
            ## average over data type (Data and Legacy)
            group_by(fYEAR, fGROUP, fDEPTH, ZONE_DEPTH, P_CODE, SITE, SITE_NO, Site, Transect) %>%
            summarise(Cover = mean(PERC_COVER)) %>%
            ungroup() %>%
            ## average over Transect
            group_by(fYEAR, fGROUP, fDEPTH, ZONE_DEPTH, P_CODE, SITE, SITE_NO, Site) %>%
            summarise(Cover = mean(Cover)) %>%
            ## average over Site/depth
            group_by(fYEAR, fGROUP, fDEPTH, ZONE_DEPTH, P_CODE, SITE, SITE_NO) %>%
            summarise(Cover = mean(Cover)) %>%
            ## average over P_CODE
            group_by(fYEAR, fGROUP, fDEPTH, ZONE_DEPTH, SITE, SITE_NO) %>%
            summarise(Cover = mean(Cover)) %>%
            ungroup() %>%
            ## Add metadata columns and rename Cover to match expected output format
            mutate(SITE = nSITE, fDEPTH = nDEPTH, REEF_ZONE = nREEF_ZONE,
                   mean = Cover, median = Cover, lower = Cover, upper = Cover) %>%
            dplyr::select(SITE, fDEPTH, REEF_ZONE, fYEAR, fGROUP, mean, median, lower, upper) %>%
            suppressMessages()
          ## Tests -----------------
          ## cellmeans %>% filter(fGROUP == 'CRUSTOSE CORALLINE ALGAE') %>% pull(fYEAR) %>% unique
          ## cellmeans %>% filter(fGROUP == 'CRUSTOSE CORALLINE ALGAE') %>% as.data.frame %>% head
          ## cellmeans %>% filter(fGROUP == 'OTHER') %>% as.data.frame %>% head
          ## -----------------------

          saveRDS(list(cellmeans=cellmeans, data = data.sub),
                  file = paste0(DATA_PATH, "modelled/", "cellmeans_RawTemporal_", DOMAIN_NAME, "_", iSITE_ZONE_DEPTH, ".RData"))
          cli::cli_progress_update(inc = 1, set = 1, force = TRUE)
          ## ----end
          rm(cellmeans)
          invisible(gc(full=TRUE))
          cli::cli_progress_done()
        }
        ## ----end
      ## }, logFile=LOG_FILE, Category='--Modelling fitting routines--',
      ## msg=paste0('Raw cell means for ', stringr::str_to_title(nSITE),'/', nDEPTH, 'm depth/',nREEF_ZONE,' zone'), return=NULL)
    } else {
      ## reefCloudPackage::ReefCloud_tryCatch({
        ## ---- reefCloudPackage::simpleINLA.sites
        {
          cli::cli_progress_bar("Simple INLA cellmeans", type = "iterator", total = 7, clear = TRUE)
          reefCloudPackage::log('INFO', logFile = LOG_FILE,
                        Category = '--Modelling (Simple INLA)--',
                        msg = paste0(DOMAIN_NAME, ": ",
                                     stringr::str_to_title(nSITE),"/",
                                     nDEPTH, "m depth/",nREEF_ZONE," zone Simple INLA cellmeans calculated"))
          ## ---- simpleINLAData.site
          ## Determine whether there are any fGROUP's that have no counts for the entire temporal series
          ## these fGROUP's should be removed prior to fitting the models
          ## and then put back in as 0 at the very end.
          counts <- data.sub %>% group_by(fYEAR, fGROUP) %>%
            summarise(Sum = sum(COUNT, na.rm=TRUE),
                      Sum1 = sum(COVER, na.rm=TRUE)) %>%
            mutate(Sum = ifelse(Sum==0 & Sum1>0, 1, Sum)) %>%
            dplyr::select(-Sum1)
          removed_fgroup <- counts %>%
            ungroup() %>%
            group_by(fGROUP) %>%
            filter(sum(Sum)==0) %>%
            mutate(mean=0, median=0, lower=0, upper = 0) %>%
            dplyr::select(-Sum)
          zero_groups <- removed_fgroup %>%
            pull(fGROUP) %>%
            unique %>%
            as.character()
          if (length(zero_groups)>0) { ## exclude groups with all 0 counts
            data.sub <- data.sub %>%
              filter(!fGROUP %in% zero_groups) %>% droplevels()
          }
          newdata <- data.sub %>%
            tidyr::expand(fYEAR, nesting(fGROUP),
                          DATA_TYPE=NA,
                          fDEPTH=NA, ZONE_DEPTH=NA, P_CODE=NA, SITE=NA, SITE_NO=NA, Site=NA, Transect=NA,
                          COUNT=NA, TOTAL=1, COVER=NA)
          i.newdata <- 1:nrow(newdata)+(nrow(data.sub))
          ## Append these onto the observed data
          data.pred <- data.sub %>%
            dplyr::select(COUNT,TOTAL,COVER,DATA_TYPE, fGROUP,fYEAR,
                          fDEPTH, ZONE_DEPTH, P_CODE, SITE, SITE_NO, Site, Transect) %>%
            bind_rows(newdata) %>%
            mutate(nYEAR=as.numeric(fYEAR),
                   nYEAR1=as.numeric(fYEAR)) %>%
            mutate(COVER = ifelse(COVER==0, 0.001, ifelse(COVER==1, 0.999, COVER)))
          ## Tests -------------------------
          ## data.pred  %>% filter(fGROUP=='OTHER') %>% as.data.frame %>% head
          ## -------------------------------

          ## Define low precision priors
          prec.prior <- list(prec = list(param = c(0.001, 0.001)))
          cli::cli_progress_update(force=TRUE)
          ## ----end
          ## ---- simpleINLAFit
          form <- Y ~ fYEAR*fGROUP +
            f(P_CODE, model='iid') +
            f(DATA_TYPE, model='iid') +
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
            if (data.sub %>% pull(!!sym(v)) %>% levels() %>% length() < 2) {
              ## fixed effects
              if (v %in% term.labels) {
                ## wch <- which(v == term.labels)  ## does not remove interaction when one of the main effects only has one level...
                wch <- grep(v, term.labels)
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

          data.pred <- as.list(data.pred)
          if (LEGACY_DATA) data.pred$Y <- with(data.pred, cbind(COUNT,COVER))
          if (!LEGACY_DATA) form <- update(form, COUNT ~.)
          family <- 'binomial'
          family <- 'binomial'
          if (LEGACY_DATA) family <- c('binomial','beta')


          mod <- INLA::inla(form,
                      family=family,
                      data=data.pred,
                      Ntrials=TOTAL,
                      control.predictor=list(link=1, compute=TRUE),
                      control.compute=list(config=TRUE)#,
                      ## silent = 2L
          )

          save(mod, file=paste0(DATA_PATH,'modelled/simplemod_',DOMAIN_NAME, "_", iSITE_ZONE_DEPTH,'.RData'))
          draws <- inla.posterior.sample(1000, result=mod, seed=123) %>% suppressWarnings()
          save(draws, file=paste0(DATA_PATH,'modelled/simpledraws_', DOMAIN_NAME, "_", iSITE_ZONE_DEPTH,'.RData'))
          cli::cli_progress_update(force=TRUE)
          ## ----end
          ## ---- simpleINLAYearGroup
          load(file=paste0(DATA_PATH,'modelled/simplemod_', DOMAIN_NAME, "_", iSITE_ZONE_DEPTH,'.RData'))
          load(file=paste0(DATA_PATH,'modelled/simpledraws_', DOMAIN_NAME, "_", iSITE_ZONE_DEPTH,'.RData'))
          ## ----long
          if (1==2) {
            wch <- grep('^f\\(.*', term.labels, perl = TRUE)
            ## remove random effects and response
            form1 <- reformulate(term.labels[-wch], response = NULL, intercept = FALSE)
            term.labels1 <- paste0('^',gsub(":",".*", attr(terms(form1), 'term.labels')),'[^:]*:[0-9]*$')
            term.labels1 <- paste0('^',mod$names.fixed,'[^:]*:[0-9]*$')

            cellmeans <- sapply(draws, function(x) x[['latent']][i.newdata])
            ## i.mod <- lapply(c('APredictor','^Predictor','Site','Transect','fYEAR[0-9]*[:][0-9]*$','fGROUP', 'P_CODE'),
            ##                 function(x) grep(x, draws[[1]]$latent %>% rownames))
            i.mod <- lapply(term.labels1, function(x) grep(x, draws[[1]]$latent %>% rownames))

            cellmeans <- sapply(draws, function(x) x[['latent']][unlist(i.mod)])

            Xmat <- model.matrix(form1, data=newdata)
            fit <- t(t(cellmeans) %*% t(Xmat))
            cellmeans.full <- newdata %>% bind_cols(as.data.frame(fit)) %>%
              pivot_longer(cols=matches("^V[0-9]*$"), names_to='Rep', values_to='Values') %>%
              mutate(value = plogis(Values))
            cellmeans <- cellmeans.full %>%
              group_by(fYEAR, fGROUP) %>%
              reefCloudPackage::mean_median_hdci(value) %>%
              suppressMessages()
          }
          ## ----end
          ## ----short
          fit <- mod$summary.fitted.values[i.newdata,]
          cellmeans <- newdata %>%
            mutate(mean=fit$mean,
                   median=fit$`0.5quant`,
                   lower=fit$`0.025quant`,
                   upper=fit$`0.975quant`) %>%
            suppressMessages() %>%
            suppressWarnings()
          ## Tests -------------------------
          ## cellmeans  %>% filter(fGROUP=='OTHER') %>% as.data.frame %>% head
          ## -------------------------------
          ## ----end
          ## correct for completely missing groups or all zeros
          ## ---- simpleINLAYearGroupCorrect
          counts <- data.sub %>% group_by(fYEAR,fGROUP) %>%
            summarise(Sum = sum(COUNT, na.rm=TRUE), Sum1 = sum(COVER, na.rm=TRUE)) %>%
            mutate(Sum = ifelse(Sum==0 & Sum1>0, 1, Sum)) %>%
            dplyr::select(-Sum1)
          cellmeans <- cellmeans %>%
            full_join(removed_fgroup) %>%
            full_join(counts) %>%
            mutate(Sum = ifelse(is.na(Sum),0, Sum)) %>%
            mutate(across(c(mean, median, lower, upper), function(x) ifelse(Sum==0, Sum, x))) %>%
            mutate(SITE = nSITE, fDEPTH = nDEPTH, REEF_ZONE = nREEF_ZONE) %>%
            dplyr::select(SITE, fDEPTH, REEF_ZONE, fYEAR, fGROUP, mean, median, lower, upper) %>%
            mutate(mean = ifelse(is.na(mean), median, mean)) %>%
            arrange(SITE, fDEPTH, REEF_ZONE, fYEAR, fGROUP)
          ## Tests -------------------------
          ## cellmeans  %>% filter(fGROUP=='OTHER') %>% as.data.frame %>% head
          ## -------------------------------
          ## dplyr::select(-Sum, -fDEPTH, -ZONE_DEPTH, -P_CODE, SITE, -SITE_NO, -Site, -Transect, -COUNT, -TOTAL)
          saveRDS(list(cellmeans=cellmeans, data = data.sub),
                  file = paste0(DATA_PATH, "modelled/", "cellmeans_simpleYearGroup_", DOMAIN_NAME, "_", iSITE_ZONE_DEPTH, ".RData"))

          cli::cli_progress_update(force=TRUE)
          ## ----end
          ## ----end
          cli::cli_progress_done()
        }
        ## ----end
      ## }, logFile=LOG_FILE, Category='--Modelling fitting routines--',
      ## msg=paste0('Simple INLA cell means for ', stringr::str_to_title(nSITE),'/', nDEPTH, 'm depth/',nREEF_ZONE,' zone'), return=NULL)
    }
  }

  ## Now compile all the results
  ## reefCloudPackage::ReefCloud_tryCatch({
    cellmeans.all <- vector('list', length=length(SITE_ZONE_DEPTHS))
    names(cellmeans.all) <- SITE_ZONE_DEPTHS
    data.orig <- data  # Preserve original data before list2env overwrites it
    for (iSITE_ZONE_DEPTH in SITE_ZONE_DEPTHS) {
      if (raw_cell_means){
        tmp <- readRDS(file = paste0(DATA_PATH, "modelled/", "cellmeans_RawTemporal_", DOMAIN_NAME, "_", iSITE_ZONE_DEPTH, ".RData"))
      } else{
        tmp <- readRDS(file = paste0(DATA_PATH, "modelled/", "cellmeans_simpleYearGroup_", DOMAIN_NAME, "_", iSITE_ZONE_DEPTH, ".RData"))
      }
      invisible(list2env(tmp, env = .GlobalEnv))
      # Extract cellmeans data to avoid conflict with cellmeans() function
      cellmeans.data <- tmp$cellmeans
      cellmeans.all[[iSITE_ZONE_DEPTH]] <- cellmeans.data %>%
        full_join(data.orig %>% dplyr::select(SITE, SITE_NO, fYEAR, DATE) %>% distinct()) %>%
        mutate(Year = as.numeric(as.character(fYEAR))) %>%
        dplyr::select(Site = SITE, Site_ID = SITE_NO,
                      Year,
                      Survey_Date = DATE,
                      Variable = fGROUP,
                      Depth = fDEPTH,
                      ReefZone = REEF_ZONE,
                      mean, lower, upper, median
        ) %>%
        suppressMessages() %>%
        suppressWarnings()
    }
    cellmeans.all <- do.call('rbind', cellmeans.all)
  ## }, logFile=LOG_FILE, Category='--Modelling fitting routines--',
  ## msg=paste0('Compile all results'), return=NULL)

  ## Write/export all results
  ## reefCloudPackage::ReefCloud_tryCatch({
    write_csv(cellmeans.all, file=paste0(AWS_OUTPUT_PATH, "output_sites.csv"), quote = "none")
    rm(data.sub, cellmeans) %>% suppressWarnings()
    invisible(gc(full=TRUE))
    cli_alert_success("Modelled data compiled into outputs")
  ## }, logFile=LOG_FILE, Category='--Modelling fitting routines--',
  ## msg=paste0('Write all results'), return=NULL)
}
