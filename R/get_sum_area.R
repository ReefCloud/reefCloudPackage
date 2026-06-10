#' @title Compute Total Reef Area by Tier
#'
#' @description
#' Calculates the total reef area grouped by the specified tier column. Optionally filters by `tier_type` (e.g., "data" or "new")
#' before performing the aggregation.
#'
#' @param post_dist_df_all A data frame containing reef area predictions and tier identifiers.
#' @param tier_col A string indicating the column name used to group tiers (e.g., "Tier5", "Tier4").
#' @param group Optional character value. If provided, filters data where `tier_type == group` before summing.
#'
#' @return A numeric vector of summed reef areas for each tier.
#'
#' @examples
#' get_sum_area(pred_df, tier_col = "Tier4")
#' get_sum_area(pred_df, tier_col = "Tier4", group = "data")
#'
#' @author Julie Vercelloni
#' @export
get_sum_area <- function(post_dist_df_all, tier_col, group = NULL) {
  if (is.null(group)) {
    sum_df <- post_dist_df_all  |> 
      dplyr::group_by(Tier5) |>
      dplyr::slice_head(n = 1) |>
      dplyr::ungroup() |>
      dplyr::group_by(!!sym(tier_col)) |>
      dplyr::summarise(sum_area = sum(reef_area), .groups = "drop")
  } else {
    sum_df <- post_dist_df_all |>
      dplyr::filter(tier_type == group) |> 
      dplyr::group_by(Tier5) |>
      dplyr::slice_head(n = 1) |>
      dplyr::group_by(!!sym(tier_col)) |>
      dplyr::summarise(sum_area = sum(reef_area), .groups = "drop")
  }

  return(sum_df)
}
