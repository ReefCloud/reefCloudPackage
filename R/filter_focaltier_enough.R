#' @title Filter Focal Tiers with Sufficient Spatio-Temporal Coverage
#'
#' @description
#' Filters `data.grp` based on spatial and temporal thresholds defined by `n.spat` (minimum spatial sites)
#' and `n.temp` (minimum temporal observations) for each level of `FOCAL_TIER`. 
#' Only tiers with enough data coverage are retained.
#'
#' @param data.grp A data frame of survey records to filter.
#' @param FOCAL_TIER A character string naming the column identifying focal tiers.
#' @param n.spat Integer. Minimum number of distinct spatial sites (lat/lon combinations) per tier.
#' @param n.temp Integer. Minimum number of distinct years per tier.
#'
#' @return A list:
#' \describe{
#'   \item{data.grp.enough}{Data with tiers meeting the spatio-temporal thresholds.}
#' }
#'
#' @examples
#' \dontrun{
#' result <- filter_focaltier_enough(data.grp, FOCAL_TIER = "Tier5", n.spat = 10, n.temp = 3)
#' result$filtered_data
#' }
#' @author Julie Vercelloni
#' @export
filter_focaltier_enough <- function(data.grp, FOCAL_TIER, n.spat, n.temp, i , N) {
   result <- status::status_try_catch(
   {
  original_tiers <- unique(data.grp[[FOCAL_TIER]])

  # Step 1: Spatial Filtering — Julie's rule is ≥ n.spat (3) sites
  tal_tier_spat <- data.grp |>
    dplyr::count(!!sym(FOCAL_TIER), LONGITUDE, LATITUDE) |>
    dplyr::count(!!sym(FOCAL_TIER)) |>
    dplyr::filter(n >= n.spat)

  data.grp.filtered <- data.grp |>
    dplyr::filter(!!sym(FOCAL_TIER) %in% tal_tier_spat[[FOCAL_TIER]]) |>
    droplevels()

  # Step 2: Temporal Filtering — Julie's rule is ≥ n.temp (2) years
  tal_tier_temp <- data.grp |>
    dplyr::count(!!sym(FOCAL_TIER), fYEAR) |>
    dplyr::count(!!sym(FOCAL_TIER)) |>
    dplyr::filter(n >= n.temp)

  data.grp.enough <- data.grp.filtered |>
    dplyr::filter(!!sym(FOCAL_TIER) %in% tal_tier_temp[[FOCAL_TIER]]) |>
    droplevels() |>
    data.frame()

  # Step 3: Identify Removed Tiers
  remaining_tiers <- unique(data.grp.enough[[FOCAL_TIER]])
  removed_tiers <- setdiff(original_tiers, remaining_tiers)

  data.grp.removed <- data.grp |>
    dplyr::filter(!!sym(FOCAL_TIER) %in% removed_tiers)

  # Update status
  old_item_name <- get_status_name(4, "filter_data_enough")
        if (!str_detect(old_item_name, "\\[")) {
        new_item_name = paste(old_item_name,"[",i," / ", N,"]")
        } else{
        new_item_name <- str_replace(old_item_name, "\\[([^\\]]*)\\]", paste("[",i," / ", N,"]"))
        }
     ## ##  status:::update_status_name (  # Disabled - private function not exportedstage = 4, item = "filter_data_enough", name = new_item_name)  # Disabled - private function

  data.grp.enough
  },
   stage_ = 4,
   name_ = "Filter data with enough coverage",
   item_ = "filter_data_enough"
   )

   return(result)
}
