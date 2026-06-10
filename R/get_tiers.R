#' @title Get tiers
#' @description Retrieve tier spatial layers from the geoserver or AWS, save locally as `.RData`, and register for reporting
#' @return NULL (saves tier spatial data for tiers 2 to 5)
#' @examples
#' get_tiers()
#' @author Murray Logan
#' @export

get_tiers <- function() {
  status::status_try_catch(
  {
    TIERS <<- NULL
    ## Extract spatial data from geoserver
    if (!DATA_FROM %in% c("SYNTHETIC","User defined")) {
      for (t in 2:5) {
        if (!DEBUG_MODE) cli_h3(paste0("Importing geojson data for tier ", t))
        tier.sf <- geojson_sf(paste0(AWS_PATH, "raw/tier-",t,".json")) %>%
          suppressMessages() %>%
          suppressWarnings()
        if (is.null(tier.sf) || nrow(tier.sf)==0) {
          next
        } else {
          if (t!=5) tier.sf <- tier.sf %>% dplyr::select(-reef_area)
          TIERS <<- c(TIERS, paste0('tier',t))
          tier.sf <- tier.sf %>%
            dplyr::mutate( !!(paste0("Tier",t)) := factor(tier_id))
          save(tier.sf, file = paste0(DATA_PATH, "/primary/tier", t, ".sf.RData"))
          if (!DEBUG_MODE) cli_h3(paste0("Make a figure for tier ", t))
        }
      }
    }
    rm(tier.sf)
    if (DATA_FROM == "S3") {
      for (i in 2:5) {
        reefCloudPackage::load_aws(file = "tiers", i, ".zip", level = "primary/GIS")
        unzip(paste0(DATA_PATH, "primary/GIS/tiers", i, ".zip"), list = FALSE,
          exdir = paste0(DATA_PATH, "primary/GIS"))
      }
    }
    if (GENERATE_REPORT) {
      ANALYSIS_STAGE <<- c(ANALYSIS_STAGE,
        list(list(type='component', value = '31b_load_tiers'))) %>%
        unique()
      save(ANALYSIS_STAGE, file=paste0(DATA_PATH, "analysis_stage.RData"))
    }
  },
  stage_ = 2,
  name_ = "Retrieve tier data",
  item_ = "retrieve_tier_data"
  )
}
