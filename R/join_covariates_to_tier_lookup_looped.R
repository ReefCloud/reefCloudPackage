#' Join Covariates to Tier Lookup
#'
#' Merges spatial tier data with the tier lookup table to attach covariates.
#'
#' @param tier.sf A spatial (`sf`) layer containing Tier5 information.
#' @return A spatial (`sf`) layer with joined covariates.
#' @author Julie Vercelloni
#' @examples
#' \dontrun{
#' tier.sf <- readRDS("data/primary/tier5.sf.RData")
#' tier.sf.joined <- join_covariates_to_tier_lookup(tier.sf)
#' }

join_covariates_to_tier_lookup_looped <- function(tier.sf, i , N) {
 #  status::status_try_catch(
   {
  load(file = paste0(DATA_PATH, 'primary/tiers.lookup.RData'))
  
  tier.sf.joined <- tier.sf |>
    dplyr::left_join(
      tiers.lookup |> dplyr::select(-reef_area, -tier_id),
      by = c("Tier5" = "Tier5")
    )

   # Update status 
    old_item_name <- get_status_name(4, "join_covariates_to_tier_lookup")
     if (!str_detect(old_item_name, "\\[")) {
        new_item_name = paste(old_item_name,"[",i," / ", N,"]")
     } else{
        new_item_name <- str_replace(old_item_name, "\\[([^\\]]*)\\]", paste("[",i," / ", N,"]"))
     }
    ## ##  status:::update_status_name (  # Disabled - private function not exportedstage = 4, item = "join_covariates_to_tier_lookup", name = new_item_name)  # Disabled - private function
     
   }#,
 #  stage_ = 4,
 #  name_ = "Join covariates to tier lookup",
 #  item_ = "join_covariates_to_tier_lookup"
 #  )
  
  return(tier.sf.joined)
}