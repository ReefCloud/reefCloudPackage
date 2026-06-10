#' Package Constants
#'
#' @description Central location for package-wide constants to avoid magic numbers (Suggestion 53)
#' @name constants
#' @keywords internal
NULL

# Geoserver Configuration Constants
GEOSERVER_DEFAULT_CHUNK_SIZE <- 5000
GEOSERVER_DEFAULT_TIMEOUT <- 300  # seconds (5 minutes)
GEOSERVER_MAX_RETRIES <- 3
GEOSERVER_BACKOFF_BASE <- 2  # seconds for exponential backoff
GEOSERVER_CACHE_DAYS <- 7    # cache validity for downloaded geoserver data

# Spatial Processing Constants
SPATIAL_SIMPLIFY_TOLERANCE <- 0.001
SPATIAL_DEFAULT_CRS <- 4326  # EPSG:4326 (WGS84)

# Data Processing Constants
BENTHIC_GROUPS <- c("CRUSTOSE CORALLINE ALGAE", "HARD CORAL", "MACROALGAE",
                    "TURF ALGAE", "SOFT CORAL", "OTHER")

# File Constants
DEFAULT_DATA_PATH <- "/data/"
DEFAULT_INPUT_DATA <- "reef_data.zip"
DEFAULT_TIER_DATA <- "tiers.zip"
