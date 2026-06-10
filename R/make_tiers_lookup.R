#' @title Make Tiers Lookup
#' @description Joins all tier spatial files (`tier2`–`tier5`) into a single `sf` object and creates a lookup table between the tiers. Saves the results as `tiers.sf.RData` and `tiers.lookup.RData`.
#' @return No return value. Creates and saves:
#' \itemize{
#'   \item \code{tiers.sf.RData} — an `sf` object of all tier joins.
#'   \item \code{tiers.lookup.RData} — a lookup table between tiers.
#' }
#' @examples
#' make_tiers_lookup()
#' # This saves processed tiers and lookup data into the `DATA_PATH`.
#' @author Murray Logan
#' @export
make_tiers_lookup <- function() {
  status::status_try_catch(
  {
    sf_use_s2(FALSE)
    load(paste0(DATA_PATH, 'primary/tier5.sf.RData'))
    tiers.sf <- tier.sf %>%
        sf::st_centroid() %>%
        dplyr::select(-one_of('Group_2')) %>%
        suppressMessages() %>%
        suppressWarnings()
    TIERS <- c('tier2', 'tier3', 'tier4', 'tier5')
    for (i in TIERS[TIERS!='tier5']) {
        load(paste0(DATA_PATH, 'primary/', i, '.sf.RData'))
        tiers.sf <- tiers.sf %>%
            sf::st_join(tier.sf %>% dplyr::select(-one_of('tier', 'tier_id', 'tir_src', 'WKT')), join = st_nearest_feature) %>%
        suppressMessages() %>%
        suppressWarnings()
    }
    save(tiers.sf, file=paste0(DATA_PATH, 'processed/tiers.sf.RData'))
    tiers.lookup <-
        tiers.sf %>%
        sf::st_drop_geometry() %>%
        dplyr::distinct() %>%
        suppressMessages() %>%
        suppressWarnings()
    save(tiers.lookup, file=paste0(DATA_PATH,'primary/tiers.lookup.RData'))
    rm(tiers.lookup, tiers.sf, TIERS)
  },
  stage_ = 3,
  name_ = "Make tiers lookup",
  item_ = "make_tiers_lookup"
  )
}
