#' @title Compile Tier-Level Fold Changes
#' @description Computes year-on-year fold changes and directional probabilities from posterior draws by tier.
#' @param pred_tierIndex Data frame with posterior predictions including year, tier, and cover values.
#' @param tier_col Character string naming the tier column (e.g., "Tier4").
#' @return List of tibbles with fold changes and probabilities for each tier level.
#' @author Julie Vercelloni
#' @export

make_contrasts <- function(pred_tierIndex, tier_col) {
    cellmeans_wide_list <- pred_tierIndex |>
      group_split(!!sym(tier_col)) |>
      map(~ .x |> 
        pivot_wider(
          names_from = fYEAR,
          values_from = cover_prop
        )
     )
  
  # Now process contrasts on each list
  predictions_list <- purrr::map(cellmeans_wide_list, ~reefCloudPackage::process_contrasts(.x, tier_col = tier_col))
  rm(cellmeans_wide_list)
  return(predictions_list)
}
