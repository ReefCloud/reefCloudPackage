#' @title Summarise model at tier level
#' @description Summarise model at tier level
#' @examples model_summariseModelTier()
#' @export
model_summariseModelTier <- function(){

  if (reefCloudPackage::isParent()) reefCloudPackage::startMatter()

  ## reefCloudPackage::ReefCloud_tryCatch({
    cli_h1("Summarising the data")
    load(file=paste0(DATA_PATH,'processed/',RDATA_FILE))

    GROUPS <- data %>% pull(fGROUP) %>% unique()
    ## GOUPS <- c("CRUSTOSE CORALLINE ALGAE","HARD CORAL","MACROALGAE","TURF ALGAE","SOFT CORAL")
    all.tiers <- vector('list', length(GROUPS))
  ## }, logFile=LOG_FILE, Category='--Summarising routines--', msg='Load data for modelling', return=NULL)


  for (GROUP in GROUPS) {
    cli_alert("Modelling for {stringr::str_to_title(GROUP)}")

    ## reefCloudPackage::ReefCloud_tryCatch({
      ## ---- RawMeans
      {
        ## ---- RawMeansTemporal
        tmp <- readRDS(file = paste0(DATA_PATH, "modelled/", "cellmeans_RawTemporal_", DOMAIN_NAME, "_", GROUP, ".RData"))
        invisible(list2env(tmp, env = .GlobalEnv))
        g <-
          cellmeans %>%
          ggplot() +
          geom_point(aes(y = Cover, x = as.numeric(as.character(fYEAR)), color = fDEPTH)) +
          geom_line(aes(y = Cover, x=as.numeric(as.character(fYEAR)), color=fDEPTH)) +
          facet_wrap(~Tier4 + Tier5, ncol=5) +
          theme_bw() +
          scale_color_discrete('Depth (m)') +
          scale_fill_discrete('Depth (m)') +
          scale_y_continuous(paste0(str_to_title(GROUP), ' cover (%)'), label = function(x) x*100) +
          theme(axis.title.x = element_blank())
        LAYOUT <- g %>% ggplot_build() %>%
          magrittr::extract2('layout') %>%
          magrittr::extract2('layout')
        ROWS <- LAYOUT %>%
          magrittr::extract2('ROW') %>%
          unique %>% length
        COLS <- LAYOUT %>%
          magrittr::extract2('COL') %>%
          unique %>% length

        ## panel_width <- 2.5
        ## panel_height <- 1.5
        ## gt <- ggplotGrob(g)
        ## panel_x <- unique(panel_cols(gt)$l)
        ## panel_y <- unique(panel_rows(gt)$t)
        ## gt$widths[panel_x] <- unit(panel_width, "inch")
        ## gt$heights[panel_y] <- unit(panel_height, "inch")
        ## gt
        ggsave(file = paste0(OUTPUT_PATH, "/figures/", "RawMeans_Tiers4-5_", DOMAIN_NAME, "_", GROUP, ".pdf"),
               plot = g, width = 2.5*ROWS, height = 1.5*COLS, scale=1) %>% suppressMessages()
        ggsave(file = paste0(OUTPUT_PATH, "/figures/", "RawMeans_Tiers4-5_", DOMAIN_NAME, "_", GROUP, ".png"),
               plot = g, width = 2.5*COLS, height = 1.5*ROWS, scale=1, dpi=72) %>% suppressMessages()
        ## ----end
        ## ---- RawMeansSpatioTemporal
        tmp <- readRDS(file = paste0(DATA_PATH, "modelled/", "cellmeans_RawSpatioTemporal_", DOMAIN_NAME, "_", GROUP, ".RData"))
        invisible(list2env(tmp, env = .GlobalEnv))
        cellmeans <-
          data %>%
          mutate(fYEAR = factor(fYEAR, levels=(sort(levels(fYEAR))))) %>%
          group_by(fYEAR, fDEPTH, REEF, Tier5) %>%
          summarise(Cover=mean(PERC_COVER)) %>%
          ungroup %>%
          group_by(fYEAR, fDEPTH, Tier5) %>%
          summarise(Cover=mean(Cover)) %>%
          ungroup %>%
          left_join(tier5.sf) %>%
          st_as_sf() %>%
          suppressMessages()
        g2 <-
          cellmeans %>%
          ggplot() +
          geom_sf(data = tier5.sf, fill = 'grey80', size=0) +
          geom_sf(data = tier4.sf, aes(color = Tier4), fill = NA) +
          scale_color_discrete('Tier 4') +
          geom_sf(aes(fill = Cover)) +
          scale_fill_gradientn(paste0(str_to_title(GROUP),' cover (%)'),
                               labels = scales::label_number(scale = 100),
                               colours = heat.colors(12)) +
          facet_grid(fDEPTH~fYEAR) +
          theme_bw()
        LAYOUT <- g2 %>% ggplot_build() %>%
          magrittr::extract2('layout') %>%
          magrittr::extract2('layout')
        ROWS <- LAYOUT %>%
          magrittr::extract2('ROW') %>%
          unique %>% length
        COLS <- LAYOUT %>%
          magrittr::extract2('COL') %>%
          unique %>% length
        ggsave(file = paste0(OUTPUT_PATH, "/figures/", "RawMeans_Spatiotemporal_Tiers4-5_", DOMAIN_NAME, "_", GROUP, ".png"),
               plot = g2, width = 4*COLS, height = 4*ROWS, scale=1, dpi=72) %>% suppressMessages()

        ## ----end
        rm(g, g2, cellmeans) %>% suppressWarnings()
      }
      ## ----end
    ## }, logFile=LOG_FILE, Category='--Summarising routines--',
    ## msg=paste0('Raw cell means for ', stringr::str_to_title(GROUP)), return=NULL)

    ## reefCloudPackage::ReefCloud_tryCatch({
      ## ---- reefCloudPackage::simpleINLA
      if (length(list.files(path = paste0(DATA_PATH, "modelled"),
                            pattern = paste0("cellmeans_simple.*", DOMAIN_NAME, "_", GROUP, "\\.RData"))) >0) {
        ## ---- simpleINLATier5
        tmp <- readRDS(file = paste0(DATA_PATH, "modelled/", "cellmeans_simpleTemporalTier5_", DOMAIN_NAME, "_", GROUP, ".RData"))
        invisible(list2env(tmp, env = .GlobalEnv))

        g <- cellmeans %>%
          ggplot(aes(y = mean, x = as.numeric(as.character(fYEAR)))) +
          geom_ribbon(aes(ymin = lower, ymax = upper), fill = "orange", color = NA, alpha = 0.5) +
          geom_line(color = "orange") +
          geom_point(color = "orange") +
          facet_wrap(~Tier4+Tier5) +
          scale_x_continuous('') +
          scale_y_continuous(paste0(str_to_title(GROUP),' cover (%)'),
                             labels = scales::label_number(scale = 100)) +
          theme_bw()

        save_graphic(plot=g,
                               file = paste0(OUTPUT_PATH, "/figures/", "simpleINLA_Tiers4-5_", DOMAIN_NAME, "_", GROUP, ".png"),
                               panel_width = unit(4, "cm"),
                               panel_height = unit(3, "cm"),
                               scale = 1,
                               dpi = 72) %>%
          suppressMessages()
        ## ----end
        ## ---- simpleINLATier5Spatiotemporal
        tmp <- readRDS(file = paste0(DATA_PATH, "modelled/", "cellmeans_simpleSpatioTemporalTier5_", DOMAIN_NAME, "_", GROUP, ".RData"))
        invisible(list2env(tmp, env = .GlobalEnv))
        ## Summary figures
        g2 <-
          cellmeans.tier5.sf %>%
          filter(!is.na(mean)) %>%
          ggplot() +
          geom_sf(data=tier4.sf, aes(fill=Tier4), alpha=0.2) +
          scale_fill_discrete('Tier 4', guide = guide_legend(ncol=2)) +
          ## guides(fill = guide_legend(nrow = 2, byrow = T)) +
          new_scale_fill() +
          geom_sf(aes(fill=mean), color=NA) +
          scale_fill_gradientn('Cover (%)', labels=scales::label_number(scale=100),
                               colors = heat.colors(12)) +
          facet_wrap(~fYEAR) +
          ## geom_sf(data=cellmeans.5.sf %>% filter(mean==max(mean)), color='red')
          theme_bw() +
          theme(legend.direction = "vertical", legend.box = "horizontal")

        save_graphic(plot=g2,
                               file = paste0(OUTPUT_PATH, "/figures/", "simpleINLA_SpatioTemporal_Tiers4-5_", DOMAIN_NAME, "_", GROUP, ".png"),
                               panel_width = unit(8, "cm"),
                               panel_height = unit(8, "cm")/reefCloudPackage::whratio(tier5.sf),
                               scale = 1,
                               dpi = 72) %>%
          suppressMessages()
        ## ----end
        ## ---- simpleINLATier4
        tmp <- readRDS(file = paste0(DATA_PATH, "modelled/", "cellmeans_simpleTemporalTier4_", DOMAIN_NAME, "_", GROUP, ".RData"))
        invisible(list2env(tmp, env = .GlobalEnv))
        g <- cellmeans.tier4.sum %>%
          ggplot(aes(y = mean, x = as.numeric(as.character(fYEAR)))) +
          geom_ribbon(aes(ymin = lower, ymax = upper), fill = "orange", color = NA, alpha = 0.5) +
          geom_line(color = "orange") +
          geom_point(color = "orange") +
          facet_wrap(~Tier4) +
          scale_x_continuous('') +
          scale_y_continuous(paste0(str_to_title(GROUP),' cover (%)'),
                             labels = scales::label_number(scale = 100)) +
          theme_bw()
        save_graphic(plot=g,
                               file = paste0(OUTPUT_PATH, "/figures/", "simpleINLA_Tier4_", DOMAIN_NAME, "_", GROUP, ".png"),
                               panel_width = unit(4, "cm"),
                               panel_height = unit(3, "cm"),
                               scale = 1,
                               dpi = 72) %>%
          suppressMessages()
        ## ----end
        ## ---- simpleINLATier4Spatiotemporal
        g2 <-
          cellmeans.tier4.sf %>%
          filter(!is.na(mean)) %>%
          ggplot() +
          geom_sf(data=tier4.sf, aes(fill=Tier4), alpha=0.2) +
          scale_fill_discrete('Tier 4', guide = guide_legend(ncol=2)) +
          new_scale_fill() +
          geom_sf(aes(fill=mean), color=NA) +
          scale_fill_gradientn('Cover (%)', labels=scales::label_number(scale=100),
                               colors = heat.colors(12)) +
          facet_wrap(~fYEAR) +
          theme_bw() +
          theme(legend.direction = "vertical", legend.box = "horizontal")

        save_graphic(plot=g2,
                               file = paste0(OUTPUT_PATH, "/figures/", "simpleINLA_SpatioTemporal_Tier4_", DOMAIN_NAME, "_", GROUP, ".png"),
                               panel_width = unit(8, "cm"),
                               panel_height = unit(8, "cm")/reefCloudPackage::whratio(tier4.sf),
                               scale = 1,
                               dpi = 72) %>%
          suppressMessages()

        ## ----end
        ## ---- simpleINLATier3
        tmp <- readRDS(file = paste0(DATA_PATH, "modelled/", "cellmeans_simpleTemporalTier3_", DOMAIN_NAME, "_", GROUP, ".RData"))
        invisible(list2env(tmp, env = .GlobalEnv))
        g <- cellmeans.tier3.sum %>%
          ggplot(aes(y = mean, x = as.numeric(as.character(fYEAR)))) +
          geom_ribbon(aes(ymin = lower, ymax = upper), fill = "orange", color = NA, alpha = 0.5) +
          geom_line(color = "orange") +
          geom_point(color = "orange") +
          facet_wrap(~Tier3) +
          scale_x_continuous('') +
          scale_y_continuous(paste0(str_to_title(GROUP),' cover (%)'),
                             labels = scales::label_number(scale = 100)) +
          theme_bw()
        save_graphic(plot=g,
                               file = paste0(OUTPUT_PATH, "/figures/", "simpleINLA_Tier3_", DOMAIN_NAME, "_", GROUP, ".png"),
                               panel_width = unit(4, "cm"),
                               panel_height = unit(3, "cm"),
                               scale = 1,
                               dpi = 72) %>%
          suppressMessages()

        ## ----end
        ## ---- simpleINLATier3Spatiotemporal
        g2 <-
          cellmeans.tier3.sf %>%
          filter(!is.na(mean)) %>%
          ggplot() +
          geom_sf(data=tier3.sf, aes(fill=Tier3), alpha=0.2) +
          scale_fill_discrete('Tier 3', guide = guide_legend(ncol=2)) +
          new_scale_fill() +
          geom_sf(aes(fill=mean), color=NA) +
          scale_fill_gradientn('Cover (%)', labels=scales::label_number(scale=100),
                               colors = heat.colors(12)) +
          facet_wrap(~fYEAR) +
          theme_bw() +
          theme(legend.direction = "vertical", legend.box = "horizontal")

        save_graphic(plot=g2,
                               file = paste0(OUTPUT_PATH, "/figures/", "simpleINLA_SpatioTemporal_Tier3_", DOMAIN_NAME, "_", GROUP, ".png"),
                               panel_width = unit(8, "cm"),
                               panel_height = unit(8, "cm")/reefCloudPackage::whratio(tier3.sf),
                               scale = 1,
                               dpi = 72) %>%
          suppressMessages()
        ## ----end
        ## ---- simpleINLATier2
        tmp <- readRDS(file = paste0(DATA_PATH, "modelled/", "cellmeans_simpleTemporalTier2_", DOMAIN_NAME, "_", GROUP, ".RData"))
        invisible(list2env(tmp, env = .GlobalEnv))
        g <- cellmeans.tier2.sum %>%
          ggplot(aes(y = mean, x = as.numeric(as.character(fYEAR)))) +
          geom_ribbon(aes(ymin = lower, ymax = upper), fill = "orange", color = NA, alpha = 0.5) +
          geom_line(color = "orange") +
          geom_point(color = "orange") +
          facet_wrap(~Tier2) +
          scale_x_continuous('') +
          scale_y_continuous(paste0(str_to_title(GROUP),' cover (%)'),
                             labels = scales::label_number(scale = 100)) +
          theme_bw()

        save_graphic(plot=g,
                               file = paste0(OUTPUT_PATH, "/figures/", "simpleINLA_Tier2_", DOMAIN_NAME, "_", GROUP, ".png"),
                               panel_width = unit(4, "cm"),
                               panel_height = unit(3, "cm"),
                               scale = 1,
                               dpi = 72) %>%
          suppressMessages()
        ## ----end
        ## ---- simpleINLATier2Spatiotemporal
        g2 <-
          cellmeans.tier2.sf %>%
          filter(!is.na(mean)) %>%
          ggplot() +
          geom_sf(data=tier2.sf, aes(fill=Tier2), alpha=0.2) +
          scale_fill_discrete('Tier 2', guide = guide_legend(ncol=2)) +
          new_scale_fill() +
          geom_sf(aes(fill=mean), color=NA) +
          scale_fill_gradientn('Cover (%)', labels=scales::label_number(scale=100),
                               colors = heat.colors(12)) +
          facet_wrap(~fYEAR) +
          theme_bw() +
          theme(legend.direction = "vertical", legend.box = "horizontal")

        save_graphic(plot=g2,
                               file = paste0(OUTPUT_PATH, "/figures/", "simpleINLA_SpatioTemporal_Tier2_", DOMAIN_NAME, "_", GROUP, ".png"),
                               panel_width = unit(8, "cm"),
                               panel_height = unit(8, "cm")/reefCloudPackage::whratio(tier2.sf),
                               scale = 1,
                               dpi = 72) %>%
          suppressMessages()
        ## ----end
        rm(g, g2, cellmeans,
           cellmeans.tier5.sf, cellmeans.tier4.sf, cellmeans.tier3.sf, cellmeans.tier2.sf,
           cellmeans.tier4.sum, cellmeans.tier3.sum, cellmeans.tier2.sum,
           tier5.sf, tier4.sf, tier3.sf, tier2.sf) %>%
          suppressWarnings()
      }
      ## ----end
    ## }, logFile=LOG_FILE, Category='--Summarising routines--',
    ## msg=paste0('Simple INLA cell means for ', stringr::str_to_title(GROUP)), return=NULL)

    ## reefCloudPackage::ReefCloud_tryCatch({
      ## ---- reefCloudPackage::meshINLA
      if (length(list.files(path = paste0(DATA_PATH, "modelled"),
                            pattern = paste0("cellmeans_mesh.*", DOMAIN_NAME, "_", GROUP, "\\.RData"))) >0) {
        ## ---- meshINLATier5TemporalFocal
        tmp <- readRDS(file = paste0(DATA_PATH, "modelled/", "cellmeans_meshTemporalTier5focal_", DOMAIN_NAME, "_", GROUP, ".RData"))
        invisible(list2env(tmp, env = .GlobalEnv))

        g <- cellmeans.tier5 %>%
          ggplot(aes(y = mean, x = as.numeric(as.character(fYEAR)))) +
          geom_ribbon(aes(ymin = lower, ymax = upper), fill = "orange", color = NA, alpha = 0.5) +
          geom_line(color = "orange") +
          geom_point(color = "orange") +
          facet_wrap(~Tier4+Tier5) +
          scale_x_continuous('') +
          scale_y_continuous(paste0(str_to_title(GROUP),' cover (%)'),
                             labels = scales::label_number(scale = 100)) +
          theme_bw()

        save_graphic(plot=g,
                               file = paste0(OUTPUT_PATH, "/figures/", "meshINLA_Tiers4-5_", DOMAIN_NAME, "_", GROUP, ".png"),
                               panel_width = unit(4, "cm"),
                               panel_height = unit(3, "cm"),
                               scale = 1,
                               dpi = 72) %>%
          suppressMessages()

        ## ----end
        ## ---- meshINLATier5Spatiotemporal
        tmp <- readRDS(file = paste0(DATA_PATH, "modelled/", "cellmeans_meshSpatioTemporalTier5_", DOMAIN_NAME, "_", GROUP, ".RData"))
        invisible(list2env(tmp, env = .GlobalEnv))
        ## Summary figures
        g2 <-
          cellmeans.tier5.sf %>%
          filter(!is.na(mean)) %>%
          ggplot() +
          geom_sf(data=tier4.sf, aes(fill=Tier4), alpha=0.2) +
          scale_fill_discrete('Tier 4', guide = guide_legend(ncol=2)) +
          ## guides(fill = guide_legend(nrow = 2, byrow = T)) +
          new_scale_fill() +
          geom_sf(aes(fill=mean), color=NA) +
          scale_fill_gradientn('Cover (%)', labels=scales::label_number(scale=100),
                               colors = heat.colors(12)) +
          facet_wrap(~fYEAR) +
          ## geom_sf(data=cellmeans.5.sf %>% filter(mean==max(mean)), color='red')
          theme_bw() +
          theme(legend.direction = "vertical", legend.box = "horizontal")

        save_graphic(plot=g2,
                               file = paste0(OUTPUT_PATH, "/figures/", "meshINLA_SpatioTemporal_Tiers4-5_", DOMAIN_NAME, "_", GROUP, ".png"),
                               panel_width = unit(8, "cm"),
                               panel_height = unit(8, "cm")/reefCloudPackage::whratio(tier5.sf),
                               scale = 1,
                               dpi = 72) %>%
          suppressMessages()
        ## ----end
        ## ---- meshINLATier4
        tmp <- readRDS(file = paste0(DATA_PATH, "modelled/", "cellmeans_meshTemporalTier4_", DOMAIN_NAME, "_", GROUP, ".RData"))
        invisible(list2env(tmp, env = .GlobalEnv))
        g <- cellmeans.tier4.sum %>%
          ggplot(aes(y = mean, x = as.numeric(as.character(fYEAR)))) +
          geom_ribbon(aes(ymin = lower, ymax = upper), fill = "orange", color = NA, alpha = 0.5) +
          geom_line(color = "orange") +
          geom_point(color = "orange") +
          facet_wrap(~Tier4) +
          scale_x_continuous('') +
          scale_y_continuous(paste0(str_to_title(GROUP),' cover (%)'),
                             labels = scales::label_number(scale = 100)) +
          theme_bw()
        save_graphic(plot=g,
                               file = paste0(OUTPUT_PATH, "/figures/", "meshINLA_Tiers4_", DOMAIN_NAME, "_", GROUP, ".png"),
                               panel_width = unit(4, "cm"),
                               panel_height = unit(3, "cm"),
                               scale = 1,
                               dpi = 72) %>%
          suppressMessages()
        ## ----end
        ## ---- meshINLATier4Spatiotemporal
        tmp <- readRDS(file = paste0(DATA_PATH, "modelled/", "cellmeans_meshSpatioTemporalTier4_", DOMAIN_NAME, "_", GROUP, ".RData"))
        invisible(list2env(tmp, env = .GlobalEnv))
        g2 <-
          cellmeans.tier4.sf %>%
          filter(!is.na(mean)) %>%
          ggplot() +
          geom_sf(data=tier4.sf, aes(fill=Tier4), alpha=0.2) +
          scale_fill_discrete('Tier 4', guide = guide_legend(ncol=2)) +
          new_scale_fill() +
          geom_sf(aes(fill=mean), color=NA) +
          scale_fill_gradientn('Cover (%)', labels=scales::label_number(scale=100),
                               colors = heat.colors(12)) +
          facet_wrap(~fYEAR) +
          theme_bw() +
          theme(legend.direction = "vertical", legend.box = "horizontal")

        save_graphic(plot=g2,
                               file = paste0(OUTPUT_PATH, "/figures/", "meshINLA_SpatioTemporal_Tiers4_", DOMAIN_NAME, "_", GROUP, ".png"),
                               panel_width = unit(8, "cm"),
                               panel_height = unit(8, "cm")/reefCloudPackage::whratio(tier4.sf),
                               scale = 1,
                               dpi = 72) %>%
          suppressMessages()

        ## ----end
        ## ---- meshINLATier3
        tmp <- readRDS(file = paste0(DATA_PATH, "modelled/", "cellmeans_meshTemporalTier3_", DOMAIN_NAME, "_", GROUP, ".RData"))
        invisible(list2env(tmp, env = .GlobalEnv))
        g <- cellmeans.tier3.sum %>%
          ggplot(aes(y = mean, x = as.numeric(as.character(fYEAR)))) +
          geom_ribbon(aes(ymin = lower, ymax = upper), fill = "orange", color = NA, alpha = 0.5) +
          geom_line(color = "orange") +
          geom_point(color = "orange") +
          facet_wrap(~Tier3) +
          scale_x_continuous('') +
          scale_y_continuous(paste0(str_to_title(GROUP),' cover (%)'),
                             labels = scales::label_number(scale = 100)) +
          theme_bw()
        save_graphic(plot=g,
                               file = paste0(OUTPUT_PATH, "/figures/", "meshINLA_Tiers3_", DOMAIN_NAME, "_", GROUP, ".png"),
                               panel_width = unit(4, "cm"),
                               panel_height = unit(3, "cm"),
                               scale = 1,
                               dpi = 72) %>%
          suppressMessages()

        ## ----end
        ## ---- meshINLATier3Spatiotemporal
        tmp <- readRDS(file = paste0(DATA_PATH, "modelled/", "cellmeans_meshSpatioTemporalTier3_", DOMAIN_NAME, "_", GROUP, ".RData"))
        invisible(list2env(tmp, env = .GlobalEnv))
        g2 <-
          cellmeans.tier3.sf %>%
          filter(!is.na(mean)) %>%
          ggplot() +
          geom_sf(data=tier3.sf, aes(fill=Tier3), alpha=0.2) +
          scale_fill_discrete('Tier 3', guide = guide_legend(ncol=2)) +
          new_scale_fill() +
          geom_sf(aes(fill=mean), color=NA) +
          scale_fill_gradientn('Cover (%)', labels=scales::label_number(scale=100),
                               colors = heat.colors(12)) +
          facet_wrap(~fYEAR) +
          theme_bw() +
          theme(legend.direction = "vertical", legend.box = "horizontal")

        save_graphic(plot=g2,
                               file = paste0(OUTPUT_PATH, "/figures/", "meshINLA_SpatioTemporal_Tiers3_", DOMAIN_NAME, "_", GROUP, ".png"),
                               panel_width = unit(8, "cm"),
                               panel_height = unit(8, "cm")/reefCloudPackage::whratio(tier3.sf),
                               scale = 1,
                               dpi = 72) %>%
          suppressMessages()
        ## ----end
        ## ---- meshINLATier2
        tmp <- readRDS(file = paste0(DATA_PATH, "modelled/", "cellmeans_meshTemporalTier2_", DOMAIN_NAME, "_", GROUP, ".RData"))
        invisible(list2env(tmp, env = .GlobalEnv))
        g <- cellmeans.tier2.sum %>%
          ggplot(aes(y = mean, x = as.numeric(as.character(fYEAR)))) +
          geom_ribbon(aes(ymin = lower, ymax = upper), fill = "orange", color = NA, alpha = 0.5) +
          geom_line(color = "orange") +
          geom_point(color = "orange") +
          facet_wrap(~Tier2) +
          scale_x_continuous('') +
          scale_y_continuous(paste0(str_to_title(GROUP),' cover (%)'),
                             labels = scales::label_number(scale = 100)) +
          theme_bw()

        save_graphic(plot=g,
                               file = paste0(OUTPUT_PATH, "/figures/", "meshINLA_Tiers2_", DOMAIN_NAME, "_", GROUP, ".png"),
                               panel_width = unit(4, "cm"),
                               panel_height = unit(3, "cm"),
                               scale = 1,
                               dpi = 72) %>%
          suppressMessages()
        ## ----end
        ## ---- meshINLATier2Spatiotemporal
        tmp <- readRDS(file = paste0(DATA_PATH, "modelled/", "cellmeans_meshSpatioTemporalTier2_", DOMAIN_NAME, "_", GROUP, ".RData"))
        invisible(list2env(tmp, env = .GlobalEnv))
        g2 <-
          cellmeans.tier2.sf %>%
          filter(!is.na(mean)) %>%
          ggplot() +
          geom_sf(data=tier2.sf, aes(fill=Tier2), alpha=0.2) +
          scale_fill_discrete('Tier 2', guide = guide_legend(ncol=2)) +
          new_scale_fill() +
          geom_sf(aes(fill=mean), color=NA) +
          scale_fill_gradientn('Cover (%)', labels=scales::label_number(scale=100),
                               colors = heat.colors(12)) +
          facet_wrap(~fYEAR) +
          theme_bw() +
          theme(legend.direction = "vertical", legend.box = "horizontal")

        save_graphic(plot=g2,
                               file = paste0(OUTPUT_PATH, "/figures/", "meshINLA_SpatioTemporal_Tiers2_", DOMAIN_NAME, "_", GROUP, ".png"),
                               panel_width = unit(8, "cm"),
                               panel_height = unit(8, "cm")/reefCloudPackage::whratio(tier2.sf),
                               scale = 1,
                               dpi = 72) %>%
          suppressMessages()
        ## ----end
        rm(g, g2, cellmeans,
           cellmeans.tier5.sf, cellmeans.tier4.sf, cellmeans.tier3.sf, cellmeans.tier2.sf,
           cellmeans.tier4.sum, cellmeans.tier3.sum, cellmeans.tier2.sum,
           tier5.sf, tier4.sf, tier3.sf, tier2.sf) %>%
          suppressWarnings()
      }
      ## ----end
    ## }, logFile=LOG_FILE, Category='--Summarising routines--',
    ## msg=paste0('Mesh INLA cell means for ', stringr::str_to_title(GROUP)), return=NULL)

    ## reefCloudPackage::ReefCloud_tryCatch({
      ## ---- meshCINLA
      if (length(list.files(path = paste0(DATA_PATH, "modelled"),
                            pattern = paste0("cellmeans_mesh.*", DOMAIN_NAME, "_", GROUP, "\\.RData"))) >0) {
        ## ---- meshCINLATier5TemporalFocal
        tmp <- readRDS(file = paste0(DATA_PATH, "modelled/",
                                     "cellmeans_meshCTemporalTier5focal_",
                                     DOMAIN_NAME, "_", GROUP, ".RData"))
        invisible(list2env(tmp, env = .GlobalEnv))

        g <- cellmeans.tier5 %>%
          ggplot(aes(y = mean, x = as.numeric(as.character(fYEAR)))) +
          geom_ribbon(aes(ymin = lower, ymax = upper), fill = "orange", color = NA, alpha = 0.5) +
          geom_line(color = "orange") +
          geom_point(color = "orange") +
          facet_wrap(~Tier4+Tier5) +
          scale_x_continuous('') +
          scale_y_continuous(paste0(str_to_title(GROUP),' cover (%)'),
                             labels = scales::label_number(scale = 100)) +
          theme_bw()

        save_graphic(plot=g,
                               file = paste0(OUTPUT_PATH, "/figures/", "meshCINLA_Tiers4-5_", DOMAIN_NAME, "_", GROUP, ".png"),
                               panel_width = unit(4, "cm"),
                               panel_height = unit(3, "cm"),
                               scale = 1,
                               dpi = 72) %>%
          suppressMessages()

        ## ----end
        ## ---- meshCINLATier5Spatiotemporal
        tmp <- readRDS(file = paste0(DATA_PATH, "modelled/",
                                     "cellmeans_meshCSpatioTemporalTier5_",
                                     DOMAIN_NAME, "_", GROUP, ".RData"))
        invisible(list2env(tmp, env = .GlobalEnv))
        ## Summary figures
        g2 <-
          cellmeans.tier5.sf %>%
          filter(!is.na(mean)) %>%
          ggplot() +
          geom_sf(data=tier4.sf, aes(fill=Tier4), alpha=0.2) +
          scale_fill_discrete('Tier 4', guide = guide_legend(ncol=2)) +
          ## guides(fill = guide_legend(nrow = 2, byrow = T)) +
          new_scale_fill() +
          geom_sf(aes(fill=mean), color=NA) +
          scale_fill_gradientn('Cover (%)', labels=scales::label_number(scale=100),
                               colors = heat.colors(12)) +
          facet_wrap(~fYEAR) +
          ## geom_sf(data=cellmeans.5.sf %>% filter(mean==max(mean)), color='red')
          theme_bw() +
          theme(legend.direction = "vertical", legend.box = "horizontal")

        save_graphic(plot=g2,
                               file = paste0(OUTPUT_PATH, "/figures/", "meshCINLA_SpatioTemporal_Tiers4-5_", DOMAIN_NAME, "_", GROUP, ".png"),
                               panel_width = unit(8, "cm"),
                               panel_height = unit(8, "cm")/reefCloudPackage::whratio(tier5.sf),
                               scale = 1,
                               dpi = 72) %>%
          suppressMessages()
        ## ----end
        ## ---- meshCINLATier4
        tmp <- readRDS(file = paste0(DATA_PATH, "modelled/",
                                     "cellmeans_meshCTemporalTier4_",
                                     DOMAIN_NAME, "_", GROUP, ".RData"))
        invisible(list2env(tmp, env = .GlobalEnv))
        g <- cellmeans.tier4.sum %>%
          ggplot(aes(y = mean, x = as.numeric(as.character(fYEAR)))) +
          geom_ribbon(aes(ymin = lower, ymax = upper), fill = "orange", color = NA, alpha = 0.5) +
          geom_line(color = "orange") +
          geom_point(color = "orange") +
          facet_wrap(~Tier4) +
          scale_x_continuous('') +
          scale_y_continuous(paste0(str_to_title(GROUP),' cover (%)'),
                             labels = scales::label_number(scale = 100)) +
          theme_bw()
        save_graphic(plot=g,
                               file = paste0(OUTPUT_PATH, "/figures/",
                                             "meshCINLA_Tiers4_",
                                             DOMAIN_NAME, "_", GROUP, ".png"),
                               panel_width = unit(4, "cm"),
                               panel_height = unit(3, "cm"),
                               scale = 1,
                               dpi = 72) %>%
          suppressMessages()
        ## ----end
        ## ---- meshCINLATier4Spatiotemporal
        tmp <- readRDS(file = paste0(DATA_PATH, "modelled/", "cellmeans_meshCSpatioTemporalTier4_", DOMAIN_NAME, "_", GROUP, ".RData"))
        invisible(list2env(tmp, env = .GlobalEnv))
        g2 <-
          cellmeans.tier4.sf %>%
          filter(!is.na(mean)) %>%
          ggplot() +
          geom_sf(data=tier4.sf, aes(fill=Tier4), alpha=0.2) +
          scale_fill_discrete('Tier 4', guide = guide_legend(ncol=2)) +
          new_scale_fill() +
          geom_sf(aes(fill=mean), color=NA) +
          scale_fill_gradientn('Cover (%)', labels=scales::label_number(scale=100),
                               colors = heat.colors(12)) +
          facet_wrap(~fYEAR) +
          theme_bw() +
          theme(legend.direction = "vertical", legend.box = "horizontal")

        save_graphic(plot=g2,
                               file = paste0(OUTPUT_PATH, "/figures/", "meshCINLA_SpatioTemporal_Tiers4_", DOMAIN_NAME, "_", GROUP, ".png"),
                               panel_width = unit(8, "cm"),
                               panel_height = unit(8, "cm")/reefCloudPackage::whratio(tier4.sf),
                               scale = 1,
                               dpi = 72) %>%
          suppressMessages()

        ## ----end
        ## ---- meshCINLATier3
        tmp <- readRDS(file = paste0(DATA_PATH, "modelled/", "cellmeans_meshCTemporalTier3_", DOMAIN_NAME, "_", GROUP, ".RData"))
        invisible(list2env(tmp, env = .GlobalEnv))
        g <- cellmeans.tier3.sum %>%
          ggplot(aes(y = mean, x = as.numeric(as.character(fYEAR)))) +
          geom_ribbon(aes(ymin = lower, ymax = upper), fill = "orange", color = NA, alpha = 0.5) +
          geom_line(color = "orange") +
          geom_point(color = "orange") +
          facet_wrap(~Tier3) +
          scale_x_continuous('') +
          scale_y_continuous(paste0(str_to_title(GROUP),' cover (%)'),
                             labels = scales::label_number(scale = 100)) +
          theme_bw()
        save_graphic(plot=g,
                               file = paste0(OUTPUT_PATH, "/figures/", "meshCINLA_Tiers3_", DOMAIN_NAME, "_", GROUP, ".png"),
                               panel_width = unit(4, "cm"),
                               panel_height = unit(3, "cm"),
                               scale = 1,
                               dpi = 72) %>%
          suppressMessages()

        ## ----end
        ## ---- meshCINLATier3Spatiotemporal
        tmp <- readRDS(file = paste0(DATA_PATH, "modelled/", "cellmeans_meshCSpatioTemporalTier3_", DOMAIN_NAME, "_", GROUP, ".RData"))
        invisible(list2env(tmp, env = .GlobalEnv))
        g2 <-
          cellmeans.tier3.sf %>%
          filter(!is.na(mean)) %>%
          ggplot() +
          geom_sf(data=tier3.sf, aes(fill=Tier3), alpha=0.2) +
          scale_fill_discrete('Tier 3', guide = guide_legend(ncol=2)) +
          new_scale_fill() +
          geom_sf(aes(fill=mean), color=NA) +
          scale_fill_gradientn('Cover (%)', labels=scales::label_number(scale=100),
                               colors = heat.colors(12)) +
          facet_wrap(~fYEAR) +
          theme_bw() +
          theme(legend.direction = "vertical", legend.box = "horizontal")

        save_graphic(plot=g2,
                               file = paste0(OUTPUT_PATH, "/figures/", "meshCINLA_SpatioTemporal_Tiers3_", DOMAIN_NAME, "_", GROUP, ".png"),
                               panel_width = unit(8, "cm"),
                               panel_height = unit(8, "cm")/reefCloudPackage::whratio(tier3.sf),
                               scale = 1,
                               dpi = 72) %>%
          suppressMessages()
        ## ----end
        ## ---- meshCINLATier2
        tmp <- readRDS(file = paste0(DATA_PATH, "modelled/", "cellmeans_meshCTemporalTier2_", DOMAIN_NAME, "_", GROUP, ".RData"))
        invisible(list2env(tmp, env = .GlobalEnv))
        g <- cellmeans.tier2.sum %>%
          ggplot(aes(y = mean, x = as.numeric(as.character(fYEAR)))) +
          geom_ribbon(aes(ymin = lower, ymax = upper), fill = "orange", color = NA, alpha = 0.5) +
          geom_line(color = "orange") +
          geom_point(color = "orange") +
          facet_wrap(~Tier2) +
          scale_x_continuous('') +
          scale_y_continuous(paste0(str_to_title(GROUP),' cover (%)'),
                             labels = scales::label_number(scale = 100)) +
          theme_bw()

        save_graphic(plot=g,
                               file = paste0(OUTPUT_PATH, "/figures/", "meshCINLA_Tiers2_", DOMAIN_NAME, "_", GROUP, ".png"),
                               panel_width = unit(4, "cm"),
                               panel_height = unit(3, "cm"),
                               scale = 1,
                               dpi = 72) %>%
          suppressMessages()
        ## ----end
        ## ---- meshCINLATier2Spatiotemporal
        tmp <- readRDS(file = paste0(DATA_PATH, "modelled/", "cellmeans_meshCSpatioTemporalTier2_", DOMAIN_NAME, "_", GROUP, ".RData"))
        invisible(list2env(tmp, env = .GlobalEnv))
        g2 <-
          cellmeans.tier2.sf %>%
          filter(!is.na(mean)) %>%
          ggplot() +
          geom_sf(data=tier2.sf, aes(fill=Tier2), alpha=0.2) +
          scale_fill_discrete('Tier 2', guide = guide_legend(ncol=2)) +
          new_scale_fill() +
          geom_sf(aes(fill=mean), color=NA) +
          scale_fill_gradientn('Cover (%)', labels=scales::label_number(scale=100),
                               colors = heat.colors(12)) +
          facet_wrap(~fYEAR) +
          theme_bw() +
          theme(legend.direction = "vertical", legend.box = "horizontal")

        save_graphic(plot=g2,
                               file = paste0(OUTPUT_PATH, "/figures/", "meshCINLA_SpatioTemporal_Tiers2_", DOMAIN_NAME, "_", GROUP, ".png"),
                               panel_width = unit(8, "cm"),
                               panel_height = unit(8, "cm")/reefCloudPackage::whratio(tier2.sf),
                               scale = 1,
                               dpi = 72) %>%
          suppressMessages()
        ## ----end
        rm(g, g2, cellmeans,
           cellmeans.tier5.sf, cellmeans.tier4.sf, cellmeans.tier3.sf, cellmeans.tier2.sf,
           cellmeans.tier4.sum, cellmeans.tier3.sum, cellmeans.tier2.sum,
           tier5.sf, tier4.sf, tier3.sf, tier2.sf) %>%
          suppressWarnings()
      }
      ## ----end
    ## }, logFile=LOG_FILE, Category='--Summarising routines--',
    ## msg=paste0('Mesh INLA cell means for ', stringr::str_to_title(GROUP)), return=NULL)
  }




  ## reefCloudPackage::ReefCloud_tryCatch({
    ## if(!reefCloudPackage::build_report(string = "\n# Fit Models {.tabset .tabset-faded}\n\n"))
    ##     cat("Modelling title will not be incorporated in the report!\n\n")
    ANALYSIS_STAGE <<- c(ANALYSIS_STAGE,
                         list(list(type='string', value = "\n# Fit Models {.tabset .tabset-faded}\n\n"))) %>%
      unique()
    save(ANALYSIS_STAGE, file=paste0(DATA_PATH, "reefCloudPackage::analysis_stage.RData"))

    ## The following code is to build the report
    ## Raw means
    if (length(list.files(path = "../output/figures",
                          pattern = "RawMeans_(Spatiotemporal_Tiers|Tiers).*png",
                          full.names = TRUE)) >0) {
      ## if(!reefCloudPackage::build_report(component = "fit_model_tier_raw"))
      ##     cat("Raw cellmeans will not be incorporated in the report!\n\n")
      ANALYSIS_STAGE <<- c(ANALYSIS_STAGE,
                           list(list(type='component', value = '44a_fit_model_tier_raw'))) %>%
        unique()
      save(ANALYSIS_STAGE, file=paste0(DATA_PATH, "reefCloudPackage::analysis_stage.RData"))
    }

    ## Simple INLA
    if (length(list.files(path = "../output/figures",
                          pattern = "simpleINLA_(Spatiotemporal_Tier|Tier).*png",
                          full.names = TRUE)) >0) {
      ## if(!reefCloudPackage::build_report(component = "fit_model_tier_simple"))
      ##     cat("Simple INLA cellmeans will not be incorporated in the report!\n\n")
      ANALYSIS_STAGE <<- c(ANALYSIS_STAGE,
                           list(list(type='component', value = '44b_fit_model_tier_simple'))) %>%
        unique()
      save(ANALYSIS_STAGE, file=paste0(DATA_PATH, "reefCloudPackage::analysis_stage.RData"))
    }

    ## INLA SPDE
    if (length(list.files(path = "../output/figures",
                          pattern = "meshINLA_(Spatiotemporal_Tier|Tier).*png",
                          full.names = TRUE)) >0) {
      ## if(!reefCloudPackage::build_report(component = "fit_model_tier_mesh"))
      ##     cat("SPDE INLA cellmeans will not be incorporated in the report!\n\n")
      ANALYSIS_STAGE <<- c(ANALYSIS_STAGE,
                           list(list(type='component', value = '44c_fit_model_tier_mesh'))) %>%
        unique()
      save(ANALYSIS_STAGE, file=paste0(DATA_PATH, "reefCloudPackage::analysis_stage.RData"))
    }

    ## INLA SPDE with covariates
    if (length(list.files(path = "../output/figures",
                          pattern = "meshCINLA_(Spatiotemporal_Tier|Tier).*png",
                          full.names = TRUE)) >0) {
      ## if(!reefCloudPackage::build_report(component = "fit_model_tier_mesh_covariates"))
      ##     cat("SPDE INLA with covariates cellmeans will not be incorporated in the report!\n\n")
      ANALYSIS_STAGE <<- c(ANALYSIS_STAGE,
                           list(list(type='component', value = '44d_fit_model_tier_mesh_covariates'))) %>%
        unique()
      save(ANALYSIS_STAGE, file=paste0(DATA_PATH, "reefCloudPackage::analysis_stage.RData"))
    }

  ## }, logFile=LOG_FILE, Category='--Summarising routines--',
  ## msg=paste0('Patch in the modelling summaries into the report'), return=NULL)


}
