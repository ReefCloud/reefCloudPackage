## INLAprep -----------------------------------------------------------------------

#' @title Function
#' @description Description
#' @param parameters description
#' @return returned arguments description
#' @examples examples
#' @export
inla_prep <- function(data.grp.tier.ready, HexPred_reefid2, i ,N) {
   result <- status::status_try_catch(
   {

data.grp.tier.ready <- data.grp.tier.ready %>%
  dplyr::mutate(fYEAR = as.character(fYEAR))

HexPred_reefid2 <- HexPred_reefid2 %>%
 dplyr:: mutate(fYEAR = as.character(fYEAR))

data.sub <- left_join(data.grp.tier.ready, HexPred_reefid2) %>%
  dplyr::select(P_CODE, reefid, Site, Transect, LONGITUDE, LATITUDE, Tier2:Tier5, fYEAR, fDEPTH, fGROUP:TOTAL,
                severity_cyc:max_dhw_lag2) %>%
  dplyr::mutate(
    fYEAR = as.factor(fYEAR),
    fDEPTH = as.factor(fDEPTH),
    reefid = as.factor(reefid),
    Site = as.factor(Site),
    Transect = as.factor(Transect)
  ) %>%
  droplevels()

 # Update status
  old_item_name <- get_status_name(4, "prep_INLA_objects")
        if (!str_detect(old_item_name, "\\[")) {
        new_item_name = paste(old_item_name,"[",i," / ", N,"]")
        } else{
        new_item_name <- str_replace(old_item_name, "\\[([^\\]]*)\\]", paste("[",i," / ", N,"]"))
        }
     ## ##  status:::update_status_name (  # Disabled - private function not exportedstage = 4, item = "prep_INLA_objects", name = new_item_name)  # Disabled - private function

list(data.sub = data.sub)
   },
   stage_ = 4,
   name_ = "Prep INLA objects",
   item_ = "prep_INLA_objects"
   )

   return(result)
}
