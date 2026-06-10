#' @title Automatically generate report
#' @description Automatically generate report
#' @examples reportGeneration()
#' @export

reportGeneration <- function(){
  # for dev:
  #   Tier4 <- "1808"
  ### Coral data
  #X_vis is data.grp
  #for each TIER4 in the folder modelled, we make a visualisation
  load(file = paste0(DATA_PATH, "modelled/",
                     "SpatioTemporal",
                     "_",FOCAL_TIER,"_", TIER, ".RData"))
  reef_tal <- data.grp %>%
    group_by(REEF, TRANSECT_NO) %>% count() %>%
    filter(n>2)

  data.grp_vis <- data.grp %>%
    filter(REEF %in% reef_tal$REEF)

  p_vis_data <- data.grp_vis%>%
    filter(Tier4==TIER) %>%
   ggplot() +
    geom_line(aes(x = fYEAR, y = (COUNT/TOTAL)*100,
                  colour=  as.factor(TRANSECT_NO),
                  group = interaction(as.factor(TRANSECT_NO),
                                      REEF)),
                                          show.legend = FALSE) +
    facet_wrap(~REEF, ncol=4) + theme_bw() +
    labs(x = "Year", y = "Coral cover", subtitle = paste("Tier",TIER4, sep="")) +
    ylab("Coral cover") + xlab("Year")+theme_bw()+
    theme(axis.text.x = element_text(size=8, angle = 90, hjust = 1),legend.position = "right",
          axis.text.y = element_text(size=8),axis.title.y=element_text(size=11),
          axis.title.x=element_text(size=11),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = 'white'),
          strip.text = element_text(size = 8, margin = margin()))


  ggsave(plot = p_vis_data,width=10, height=12,
         file = "../figures report/CoralData.png")
  ### Disturbance data

  #### Cyclone exposure

  #### Heat stress

  #### QA/QC: quality control in markdown, under modelling

  ### Spatial scales of aggregation
  #visualisation boundary Tier4: onglet viz of model performance
  map_tier5 <- ggplot() +
    geom_sf(data = tier4 %>% filter(tier_id == TIER4), fill="transparent")+
    geom_sf(data = tier5_all_centr, col="gray90", size=.8) +
    geom_sf(data = st_as_sf(pred_with_data), fill = "grey", col= "black", size = 4.3) +
    theme_bw()

  ## Results
  ### Tier5 level: onbet spatio temportal model at tier 5 and p_viz_final  dans viz of model perofmrance

  ### Tier4 level: onglet scaling up at tier4 level

  ### Effect of disturbances: onglet effect of disturbance
  # to do: coef_table_fixed %>%
  #   mutate(param = case_when(param == "MaxDHW" ~ "Heat stress",
  #                            param == "LagMaxDHW.1" ~ "Heat stress (lag1)",
  #                            param == "LagMaxDHW.2" ~ "Heat stress (lag2)",
  #                            param == "Wave_hours.weighted." ~ "Cyclone exposure",
  #                            param == "LagWave_hours.weighted..1" ~ "Cyclone exposure (lag1)",
  #                            param == "LagWave_hours.weighted..2" ~ "Cyclone exposure (lag2)"))

  ## model diagnostics: onglet: appelle le model object /!\ ==>save le model object

  #### generer le quarto
}
