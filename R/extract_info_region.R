#' Extract and Summarise Regional Information from Model Outputs
#'
#' This function summarises regional information from the model output data by tier. 
#' It computes total area, year range, proportion of observations from data vs modelled ("new") tiers, 
#' and contribution from each model type (e.g., FRK or INLA).
#'
#' @param post_dist_df_all A data frame of model outputs including predicted values, model types, tiers, and other metadata.
#' @param tier_col A character string specifying the tier level column to group and summarise by (e.g., "Tier4").
#'
#' @return A data frame summarising the tier-wise area, temporal coverage, and model/data contributions.
#' @author Julie Vercelloni
#' @export

extract_info_region <- function(post_dist_df_all, tier_col) {
## Extract sum_area (geographic; same across benthic groups so joined on tier only)

sum_area <- reefCloudPackage::get_sum_area(post_dist_df_all, tier_col) %>%
  arrange(!!sym(tier_col))

## Extract year range — per Group + tier

year_range <- post_dist_df_all %>% dplyr::group_by(Group, !!sym(tier_col)) %>%
  dplyr::mutate(
    fYEAR_numeric = as.numeric(as.character(fYEAR))) %>%
  dplyr::summarize(
    year_range = paste0(min(fYEAR_numeric, na.rm = TRUE), "–", max(fYEAR_numeric, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  dplyr::select(Group, !!sym(tier_col), year_range) %>%
  dplyr::arrange(Group, !!sym(tier_col))


# Extract % of data tier — per Group
data_perc <- post_dist_df_all %>%
  dplyr::group_by(Group, !!sym(tier_col)) %>%
  dplyr::count(tier_type) %>%
  dplyr::mutate(prop = (n / sum(n))*100) %>%
  dplyr::select(Group, !!sym(tier_col), tier_type, prop) %>%
  tidyr::pivot_wider(names_from = tier_type, values_from = prop, values_fill = 0) %>%
  dplyr::arrange(Group, !!sym(tier_col))

# Extract % of model types — per Group
model_perc <- post_dist_df_all %>%
  dplyr::group_by(Group, !!sym(tier_col)) %>%
  dplyr::count(model_name) %>%
  dplyr::mutate(prop = (n / sum(n))*100) %>%
  dplyr::select(Group, !!sym(tier_col), model_name, prop) %>%
  tidyr::pivot_wider(names_from = model_name, values_from = prop, values_fill = 0) %>%
  dplyr::arrange(Group, !!sym(tier_col))


# Join — sum_area on tier only (geographic), the rest on (Group, tier_col)
all_info <- year_range %>%
  dplyr::left_join(sum_area, by = tier_col) %>%
  dplyr::left_join(data_perc, by = c("Group", tier_col)) %>%
  dplyr::left_join(model_perc, by = c("Group", tier_col)) %>%
  dplyr::rename(
    Size.area = sum_area,
    Year.range = year_range
  )

# data.tier / new.tier columns may be missing if no observations of that type
if (!"data" %in% names(all_info)) all_info$data <- 0
if (!"new" %in% names(all_info)) all_info$new <- 0
all_info <- all_info %>% dplyr::rename(data.tier = data, new.tier = new)

# ensure FRK and INLA exist
if (!"FRK" %in% names(all_info)) all_info$FRK <- 0
if (!"INLA" %in% names(all_info)) all_info$INLA <- 0

all_info <- all_info %>%
  dplyr::rename(FRK.prop = FRK, INLA.prop = INLA) %>%
  dplyr::select(Group, !!sym(tier_col), Size.area, Year.range, data.tier, new.tier, FRK.prop, INLA.prop, dplyr::everything())

return(all_info)
}
