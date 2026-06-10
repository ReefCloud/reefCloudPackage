
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
simpleINLA_data <- function(data.sub, GROUP, TIER) {
  ## ---- simpleINLAData
  tiers.lookup <- reefCloudPackage::get_tiers_lookup()
  tier5.sf <- get(load(paste0(DATA_PATH,'primary/tier5.sf.RData')))
  tier4.sf <- get(load(paste0(DATA_PATH,'primary/tier4.sf.RData')))
  ## Generate the Tier5 prediction grid
  newdata.tier5 <- data.sub %>%
    tidyr::expand(fYEAR, nesting(Tier4, Tier5),
                  fDEPTH=NA, P_CODE = NA, Site=NA, Transect=NA,
                  COUNT=NA, TOTAL=1, DATA_TYPE = NA, COVER = NA)
  i.newdata.tier5 = 1:nrow(newdata.tier5)+(nrow(data.sub))
  ## Generate the Tier4 prediction grid
  newdata.tier4 <- data.sub %>%
    tidyr::expand(fYEAR, Tier4, Tier5=NA,
                  fDEPTH=NA, P_CODE = NA, Site=NA, Transect=NA,
                  COUNT=NA, TOTAL=1, DATA_TYPE = NA, COVER = NA)
  i.newdata.tier4 = 1:nrow(newdata.tier4)+(nrow(data.sub)+nrow(newdata.tier5))
  ## Append these onto the observed data
  data.pred <- data.sub %>%
    dplyr::select(COUNT,TOTAL,COVER,P_CODE,DATA_TYPE,fYEAR,
                  fDEPTH,Tier4,Tier5, Site, Transect) %>%
    bind_rows(newdata.tier5) %>%
    bind_rows(newdata.tier4) %>%
    mutate(nYEAR=as.numeric(fYEAR),
           nYEAR1=as.numeric(fYEAR),
           nTier4=as.numeric(as.factor(Tier4)),
           nTier5=as.numeric(as.factor(Tier5))) %>%
    mutate(COVER = ifelse(COVER==0, 0.001, ifelse(COVER==1, 0.999, COVER)))
  ## Define low precision priors
  prec.prior <- list(prec = list(param = c(0.001, 0.001)))

  list(newdata.tier5 = newdata.tier5,
       i.newdata.tier5 = i.newdata.tier5,
       newdata.tier4 = newdata.tier4,
       i.newdata.tier4 = i.newdata.tier4,
       data.pred = data.pred,
       prec.prior = prec.prior,
       tier5.sf = tier5.sf,
       tier4.sf = tier4.sf)
  ## ----end
}
