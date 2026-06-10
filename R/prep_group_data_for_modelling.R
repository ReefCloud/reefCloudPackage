#' @title Group data for model
#' @description Prepare fGROUP for modelling
#' @param data data set
#' @param GROUP string, name of group
#' @return grouped data, arranged by Tier4, Tier5, Site, Transect, and descending fYEAR
#' @examples prep_group_data_for_modelling(data, GROUP)
#' @export
prep_group_data_for_modelling <- function(data, GROUP) {
  data.grp <- status::status_try_catch(
  {
    data %>%
      dplyr::filter(fGROUP == GROUP) %>%
      droplevels() %>%
      dplyr::mutate(
        Tier5 = factor(Tier5),
        Tier4 = factor(Tier4),
        Tier3 = factor(Tier3),
        Tier2 = factor(Tier2),
        P_CODE = factor(P_CODE),
        Site = factor(paste(Tier5, SITE_NO)),
        Transect = factor(paste(Site, TRANSECT_NO))) %>%
      dplyr::arrange(Tier4, Tier5, Site, Transect, desc(as.numeric(as.character(fYEAR)))) %>%
      dplyr::mutate(fYEAR = factor(fYEAR, levels=unique(fYEAR)))
  },
  stage_ = 4,
  name_ = "Filter for benthic group",
  item_ = "filter_for_benthic_group"
  )
  return(data.grp)
}
