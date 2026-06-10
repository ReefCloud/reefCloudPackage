##' Fill in data gaps
##'
##' The data are record positive - that is, they only indicate what is
##' there and what is not there. For the purposes of the models, if a
##' benthic group is not encountered in a particular report year
##' within a specific sampling unit, then it should be recorded as 0.
##' So the following is intended to generate a full set benthic groups
##' within each spatio-temporal sampling unit...
##' @title Fill data gaps 
##' @param data
##' A dataframe (or tibble) containing the benthic data
##' @return dataframe 
##' @author Murray
fill_gaps <- function(data) {
  status::status_try_catch(
  {
    ## GROUPS <- data %>% pull(fGROUP) %>% unique
    GROUPS <- c("CRUSTOSE CORALLINE ALGAE","HARD CORAL","MACROALGAE","TURF ALGAE","SOFT CORAL","OTHER")
    data.filler <- data %>%
      dplyr::select(P_CODE, #ID,
        DATA_TYPE, REEF,
        SITE, SITE_NO, TRANSECT_NO,
        fDEPTH, ZONE_DEPTH, fDEPTH,
        VARIABLE,
        REPORT_YEAR, fYEAR, DATE, TOTAL) %>%
      distinct() %>%
      crossing(fGROUP = GROUPS)
    ## Tests -----------------
    ## data.filler %>% filter(DATA_TYPE == 'Data', SITE == 'Angaur', fGROUP == 'CRUSTOSE CORALLINE ALGAE') %>% pull(REPORT_YEAR) %>% unique
    ## data.filler %>% filter(DATA_TYPE == 'Legacy', SITE == 'Angaur', fGROUP == 'CRUSTOSE CORALLINE ALGAE') %>% pull(REPORT_YEAR) %>% unique
    ## data.filler %>% filter(DATA_TYPE == 'Data', SITE == 'Ngaraard', fGROUP == 'OTHER') %>% pull(REPORT_YEAR) %>% unique
    ## data.filler %>% filter(DATA_TYPE == 'Legacy', SITE == 'Ngaraard', fGROUP == 'OTHER') %>% pull(REPORT_YEAR) %>% unique
    ## data.filler %>% filter(DATA_TYPE == 'Legacy', SITE == 'Ngaraard', fGROUP == 'OTHER') %>% as.data.frame %>% head
    ## -----------------------

    data <- data %>%
      full_join(data.filler) %>%
      group_by(P_CODE, #ID,
        DATA_TYPE, REEF,
        SITE, SITE_NO, TRANSECT_NO,
        fDEPTH, ZONE_DEPTH,
        VARIABLE,
        REPORT_YEAR, fYEAR, DATE) %>%
      mutate(COUNT = ifelse(is.na(COUNT), 0, COUNT),
        TOTAL = max(TOTAL, na.rm = TRUE),
        TOTAL = ifelse(is.numeric(TOTAL) & is.infinite(TOTAL) & DATA_TYPE == 'Legacy', NA, TOTAL),
        PERC_COVER = ifelse(TOTAL == 0 | is.na(TOTAL), NA_real_, COUNT/TOTAL),
        REPORT_YEAR = ifelse(is.na(REPORT_YEAR), as.numeric(as.character(fYEAR)), REPORT_YEAR),
        ## fGROUP = ifelse(!(fGROUP %in% GROUPS), "OTHER", fGROUP)) %>%     # the OTHER group is already in the data
        ) %>%
      filter(fGROUP %in% GROUPS) %>% droplevels() %>%
      ungroup %>%
      group_by(P_CODE, #ID,
        DATA_TYPE, REEF,
        SITE, SITE_NO, TRANSECT_NO,
        fDEPTH, ZONE_DEPTH,
        VARIABLE, fGROUP,
        REPORT_YEAR, fYEAR, DATE) %>%
      summarise(COUNT = sum(COUNT),
        TOTAL = max(TOTAL, na.rm = TRUE),
        TOTAL = ifelse(is.numeric(TOTAL) & is.infinite(TOTAL) & DATA_TYPE == 'Legacy', NA, TOTAL),
        PERC_COVER = sum(PERC_COVER),
        COVER = mean(COVER)) %>%
      mutate(COUNT = ifelse(!is.na(COVER), NA, COUNT),                    ## for legacy data that only provides
        TOTAL = ifelse(!is.na(COVER), NA, TOTAL),                    ## COVER, reset COUNT, TOTAL and
        PERC_COVER = ifelse(!is.na(COVER), NA, PERC_COVER)) %>%      ## PERC_COVER to NA
      filter(TOTAL != 0 | (is.na(TOTAL) & !is.na(COVER))) %>%
      ungroup %>%
      droplevels
    ## Tests -----------------
    ## data %>% filter(DATA_TYPE == 'Data', SITE == 'Angaur', fGROUP == 'CRUSTOSE CORALLINE ALGAE') %>% pull(REPORT_YEAR) %>% unique
    ## data %>% filter(DATA_TYPE == 'Legacy', SITE == 'Angaur', fGROUP == 'CRUSTOSE CORALLINE ALGAE') %>% pull(REPORT_YEAR) %>% unique
    ## data %>% filter(DATA_TYPE == 'Legacy', SITE == 'Angaur', fGROUP == 'CRUSTOSE CORALLINE ALGAE') %>% as.data.frame %>% head
    ## data %>% filter(DATA_TYPE == 'Data', SITE == 'Ngaraard', fGROUP == 'OTHER') %>% pull(REPORT_YEAR) %>% unique
    ## data %>% filter(DATA_TYPE == 'Legacy', SITE == 'Ngaraard', fGROUP == 'OTHER') %>% pull(REPORT_YEAR) %>% unique
    ## data %>% filter(DATA_TYPE == 'Legacy', SITE == 'Ngaraard', fGROUP == 'OTHER') %>% as.data.frame %>% head
    ## -----------------------

    ## if (DEBUG_MODE) {
    ##   reefCloudPackage::change_status(stage = paste0("STAGE", CURRENT_STAGE), item = "Filling data gaps",
    ##                            status = "success")
    ##   reefCloudPackage::openingBanner()
    ## }
    ## },
    ## logFile=LOG_FILE,
    ## Category='--Site level processing routines--',
    ## msg='Fill in the data gaps',
    ## return=NULL,
    ## stage = paste0("STAGE", CURRENT_STAGE),
    ## item = "Filling data gaps")
    
  },
  stage_ = 3,
  name_ = "Fill data gaps",
  item_ = "fill_data_gaps"
  )
  return(data)
}
