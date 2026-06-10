#' @title Prepare Data for Analyses
#' @description Processes and summarizes benthic monitoring data, including grouping, aggregation, and formatting required for downstream analysis. Saves both `.RData` and `.csv` outputs.
#' @param data A data frame containing the benthic data with required fields (e.g., REPORT_YEAR, GROUP_DESC, SITE_DEPTH, etc.).
#' @return A cleaned and formatted `data.frame` ready for analysis and reporting.
#' @examples
#' \dontrun{
#' load("raw_benthic_data.RData")
#' processed_data <- prepare_data(benthic_data)
#' }
#' @author Murray Logan
#' @export
prepare_data <- function(data) {
  status::status_try_catch(
  {
    data %>%
      dplyr::mutate(
        P_CODE = factor(P_CODE),
        ID = factor(ID),
        fYEAR = factor(REPORT_YEAR),
        SITE_DEPTH = ifelse(is.na(SITE_DEPTH),'_', SITE_DEPTH),
        fDEPTH = factor(SITE_DEPTH),
        across(c(SITE_NO, TRANSECT_NO, fYEAR, fDEPTH), function(x) factor(as.character(x))),
        DATE = as.Date(SURVEY_DATE, format = '%Y-%m-%d %h:%m:%s'),
        fGROUP = factor(GROUP_DESC)) %>%
      dplyr::group_by(P_CODE, REEF, SITE_NO, TRANSECT_NO,
        DATA_TYPE,
        LATITUDE, LONGITUDE,
        across(matches("^Tier[2345]$")),
        REPORT_YEAR, DATE, fYEAR, fDEPTH, REEF_ZONE,
        fGROUP, GROUP_DESC) %>%
      dplyr::summarise(COUNT = n(),
        COVER = mean(COVER)/100) %>%
      dplyr::ungroup(fGROUP, GROUP_DESC) %>%
      dplyr::mutate(TOTAL=sum(COUNT),
        PERC_COVER=COUNT/TOTAL,
        ZONE_DEPTH=interaction(REEF_ZONE, fDEPTH)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(REPORT_YEAR)) %>% droplevels() %>%
      dplyr::mutate(COUNT = ifelse(!is.na(COVER), NA, COUNT),
        TOTAL = ifelse(!is.na(COVER), NA, TOTAL),
        PERC_COVER = ifelse(!is.na(COVER), NA, PERC_COVER)) %>%
      suppressMessages() %>%
      suppressWarnings() ->  
      data
    save(data, file=paste0(DATA_PATH, "processed/", RDATA_FILE))
    write_csv(data %>% dplyr::select(-fYEAR),
      file = paste0(AWS_OUTPUT_PATH, gsub('.csv','_tier.csv', CSV_FILE)))
    if (!DEBUG_MODE) cli_alert_success("Benthic data successfully processed")
    if (GENERATE_REPORT) {
      ANALYSIS_STAGE <<- c(ANALYSIS_STAGE,
        list(list(type='component', value = '32_prepare_tier'))) %>%
        unique()
      save(ANALYSIS_STAGE, file=paste0(DATA_PATH, "analysis_stage.RData"))
    }
    return(data)
  },
  stage_ = 3,
  name_ = "Prepare data",
  item_ = "prepare_data"
  )
}
