cyclone_exposure_gif <- function(){
  # Save all the years in a folder calls EXTRA [NEED TO BE CHANGED]
  #[IF YES, MAYBE NEED TO CREATE SUBFOLDERS FOR EACH TIER4 with output as sub-subfolders]

  yy <- unique(HexPred_reefid2$fYEAR)
  plot.list <- list()

  for ( i in 1: length(unique(HexPred_sf_raw$fYEAR))){
    plot.list[[i]] <- tm_shape(HexPred_sf_raw %>% filter(fYEAR == yy[i])) +
      tm_polygons("Wave_hours(weighted)",title=paste0("Wave intensity in ", yy[i]),legend.hist=TRUE,
                  breaks = seq(0,plyr::round_any(max(HexPred_sf_raw$`Wave_hours(weighted)`), 1, f = ceiling), by = 4), palette = "-magma") +
      tm_layout(legend.outside = TRUE, legend.outside.position = "right", legend.hist.size = 1, legend.hist.height = 0.4,
                legend.hist.width = 0.7)
    tmap_save(plot.list[[i]], file = sprintf('../extra/Cycl%02d.png', i) )
  }

  # Create the GIF
  ###############################################################################################
  ###############################################################################################
  ############################################################################################### FUNCTIONS TO BE ADDED SOMEWHERE ELSE

  GIF.convert <- function(x, output) #Create a function to read, animate and convert the files to gif
  {
    image_read(x) %>%
      image_animate(fps = 1) %>%
      image_write(output)
  }
  ###############################################################################################
  ###############################################################################################
  ###############################################################################################

  png.files_cycl <- sprintf("../extra/Cycl%02d.png", 1:length(unique(HexPred_reefid2$fYEAR)))
  GIF.convert(png.files_cycl, output = "../output/animation_cyclone.gif")
}
