#' Filter FOCAL_TIERs with Insufficient Spatio-Temporal Coverage
#'
#' Filters data based on minimum spatial and temporal observations per FOCAL_TIER.
#' Tiers with too few observations across space or years are removed and listed separately.
#'
#' @title Filter Focal Tiers
#' @param data.grp A data frame containing survey data to be filtered.
#' @param FOCAL_TIER A character string indicating the column name representing the tier.
#' @return A list with two elements:
#'   \describe{
#'     \item{filtered_data}{The cleaned data frame with sufficient spatial and temporal coverage.}
#'     \item{removed_tiers}{A data frame of observations removed due to low coverage.}
#'   }
#' @examples
#' \dontrun{
#' result <- filter_focaltier(data.grp, "Tier5")
#' filtered <- result$filtered_data
#' removed <- result$removed_tiers
#' }
#' @author Julie Vercelloni

filter_focaltier <- function(data.grp, FOCAL_TIER, n.spat, n.temp) {
  result <- status::status_try_catch(
   {
  original_tiers <- unique(data.grp[[FOCAL_TIER]])

  # Step 1: Spatial Filtering
  tal_tier_spat <- data.grp |>
    dplyr::count(!!sym(FOCAL_TIER), LONGITUDE, LATITUDE) |>
    dplyr::count(!!sym(FOCAL_TIER)) |>
    dplyr::filter(n > n.spat)

  data.grp.filtered <- data.grp |>
    dplyr::filter(!!sym(FOCAL_TIER) %in% tal_tier_spat[[FOCAL_TIER]]) |>
    droplevels()

  # Step 2: Temporal Filtering
  tal_tier_temp <- data.grp |>
    dplyr::count(!!sym(FOCAL_TIER), fYEAR) |>
    dplyr::count(!!sym(FOCAL_TIER)) |>
    dplyr::filter(n > n.temp)

  data.grp.filtered <- data.grp.filtered |>
    dplyr::filter(!!sym(FOCAL_TIER) %in% tal_tier_temp[[FOCAL_TIER]]) |>
    droplevels() |>
    data.frame()

  # Step 3: Identify Removed Tiers
  remaining_tiers <- unique(data.grp.filtered[[FOCAL_TIER]])
  removed_tiers <- setdiff(original_tiers, remaining_tiers)

  data.grp.removed <- data.grp |>
    dplyr::filter(!!sym(FOCAL_TIER) %in% removed_tiers)

  list(filtered_data = data.grp.filtered, removed_tiers = data.grp.removed)
  },
   stage_ = 4,
   name_ = "Filter for data coverage",
   item_ = "filter_data_coverage"
   )

   return(result)
}
