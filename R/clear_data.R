#' @title clear_data
#' @description The following function clears all data
#' @examples clear_data()
#' @export
clear_data <- function(){
  data_path <- if (exists("DATA_PATH", envir = .GlobalEnv)) DATA_PATH else DEFAULT_DATA_PATH

  ## Clear the data/primary folder
  unlink(paste0(data_path, "primary/*.csv"))
  unlink(paste0(data_path, "primary/*.zip"))
  unlink(paste0(data_path, "primary/*.RData"))
  unlink(paste0(data_path, "primary/*.geojson"))
  unlink(paste0(data_path, "primary/*.json"))
  unlink(paste0(data_path, "primary/GIS/*.*"), recursive = TRUE)

  unlink(paste0(data_path, "processed/*.*"), recursive = TRUE)
  unlink(paste0(data_path, "modelled/*.*"), recursive = TRUE)
  unlink(paste0(data_path, "summarised/*.*"), recursive = TRUE)

  unlink("../output/figures/*.*", recursive = TRUE)

  unlink("../docs/Reports/*.html")
  unlink(paste0(data_path, "model_stage.RData"))
  unlink(paste0(data_path, "reefCloudPackage::analysis_stage.RData"))
}
