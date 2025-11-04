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
    mutate(iter = seq_len(n())) |>
    pivot_longer(cols = contains("20"), names_to = "year") |>
    mutate(year = as.integer(year)) |>
    arrange(year, iter) |>
    group_by(iter) |>
    mutate(diff = value / lag(value, n = 1),
           diff_id = factor(paste0("diff_", seq_len(n())))) |>
    ungroup()

  max_iter <- max(predictions_i$iter)

  plot_data <- predictions_i |>
    drop_na() |>
    mutate(cat_up = ifelse(diff > 1, 1, 0),
           cat_down = ifelse(diff < 1, 1, 0)) |>
    group_by(diff_id, year) |>
    summarise(prob_up = sum(cat_up) / max_iter,
              prob_down = sum(cat_down) / max_iter,
              .groups = "drop")

  fold_change <- predictions_i |>
    drop_na() |>
    group_by(year, diff_id) |>
    summarise(fold_change = mean(diff), .groups = "drop") |>
    left_join(plot_data, by = join_by(year, diff_id))

  direction_arrow <- fold_change |>
    group_by(year) |>
    mutate(arrow = case_when(
      fold_change > 1 & prob_up >= 0.9 ~ "Up",
      fold_change < 1 & prob_down >= 0.9 ~ "Down",
      TRUE ~ "Flat"
    )) |>
    select(year, fold_change, prob_up, prob_down, arrow)

  predictions_i <- predictions_i |>
    filter(!is.na(value)) |>
    group_by(year, !!sym(tier_col), model_name) |>
    ggdist::median_hdci(value) |>
    select(-.width, -.interval) |>
    left_join(direction_arrow, by = "year")
  
  return(predictions_i)
  rm(plot_data, fold_change, direction_arrow)
}
