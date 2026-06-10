
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
make_tiers_figures <- function(tier.sf) {
    tier2.sf <- reefCloudPackage::loadRData(file = paste0(DATA_PATH, "primary/tier2.sf.RData"))
    coast <- rnaturalearth::ne_coastline(returnclass = "sf", scale='medium') %>% st_make_valid()
    land <- rnaturalearth::ne_countries(returnclass = "sf", scale='medium') %>% st_make_valid()

    ## cat(tier.sf)
    ## Get the tier name
    names <- tier.sf %>% colnames
    name <- names[grepl('Tier[0-9]', names)]
    g1 <- ggplot() +
	geom_sf(data=coast) +
	geom_sf(data=tier.sf, fill='orange') +
	theme_bw()
    g2 <- ggplot() +
	geom_sf(data=land %>% st_intersection(tier2.sf) %>% suppressMessages() %>% suppressWarnings()) +
	geom_sf(data=tier.sf, fill='orange', alpha=0.3) +
	theme_bw()
    if (name %in% c('Tier2', 'Tier3')) {
        ggsave(filename = paste0(OUTPUT_PATH, "figures/", name ,".png"),
               g1 + g2, width=10, height=5, dpi=72) %>% suppressMessages() %>% suppressWarnings()
    } else {
        ggsave(filename = paste0(OUTPUT_PATH, "figures/", name ,".png"),
               g2, width=7, height=7, dpi=72) %>% suppressMessages() %>% suppressWarnings()
    }
}
