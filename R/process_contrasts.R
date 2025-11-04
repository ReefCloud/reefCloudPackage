#' @title Compute Annual Fold Changes and Probabilities
#' @description Computes year-on-year fold changes and the probabilities of increase or decrease
#'              based on posterior distributions, for each level of a specified tier.
#' @param cellmeans_wide A wide-format tibble of posterior predictions with years as column names.
#' @param tier_col A character string indicating the column name to group by (e.g., "Tier4").
#' @return A tibble containing median estimates, fold changes, and probability annotations per year.
#' @author Julie Vercelloni
#' @export

process_contrasts <- function(cellmeans_wide, tier_col) {
  predictions_i <- cellmeans_wide |>
    dplyr::mutate(iter = seq_len(n())) |>
    tidyr::pivot_longer(cols = contains("20"), names_to = "year") |>
    dplyr::mutate(year = as.integer(year)) |>
    dplyr::arrange(year, iter) |>
    dplyr::group_by(iter) |>
    dplyr::mutate(diff = value / lag(value, n = 1),
           diff_id = factor(paste0("diff_", seq_len(n())))) |>
    dplyr::ungroup()

  max_iter <- max(predictions_i$iter)

  plot_data <- predictions_i |>
    tidyr::drop_na() |>
    dplyr::mutate(cat_up = ifelse(diff > 1, 1, 0),
           cat_down = ifelse(diff < 1, 1, 0)) |>
    dplyr::group_by(diff_id, year) |>
    dplyr::summarise(prob_up = sum(cat_up) / max_iter,
              prob_down = sum(cat_down) / max_iter,
              .groups = "drop")

  fold_change <- predictions_i |>
    tidyr::drop_na() |>
    dplyr::group_by(year, diff_id) |>
    dplyr::summarise(fold_change = mean(diff), .groups = "drop") |>
    dplyr::left_join(plot_data, by = join_by(year, diff_id))

  direction_arrow <- fold_change |>
    dplyr::group_by(year) |>
    dplyr::mutate(arrow = case_when(
      fold_change > 1 & prob_up >= 0.9 ~ "Up",
      fold_change < 1 & prob_down >= 0.9 ~ "Down",
      TRUE ~ "Flat"
    )) |>
    dplyr::select(year, fold_change, prob_up, prob_down, arrow)

  predictions_i <- predictions_i |>
    dplyr::filter(!is.na(value)) |>
    dplyr::group_by(year, !!sym(tier_col), model_name) |>
    ggdist::median_hdci(value) |>
    dplyr::select(-.width, -.interval) |>
    dplyr::left_join(direction_arrow, by = "year")
  
  rm(plot_data, fold_change, direction_arrow)
  return(predictions_i)
}
