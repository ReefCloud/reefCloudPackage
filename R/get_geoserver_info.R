#' @title Get Geoserver Info
#' @description Retrieves metadata from the geoserver.
#' This function loads available reefCloud layers and geoserver URL into the global environment.
#' @return NULL
#' @author Murray Logan
#' @export
#' @examples
#' get_geoserver_info()
#' # This will load `rc_client` in the global environment
get_geoserver_info <- function() {
  # Get Geoserver info
  rc_url <-"https://geoserver.apps.aims.gov.au/reefcloud/ows"

  # Configure GDAL HTTP options to pass browser-like headers
  # This ensures headers are used for all underlying HTTP requests made by GDAL/OGR
  header_file <- tempfile(fileext = ".txt")
  writeLines(c(
    "User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
    "Content-Type: application/xml",
    "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8",
    "Accept-Language: en-US,en;q=0.9",
    "Accept-Encoding: gzip, deflate, br",
    "Connection: keep-alive"
  ), header_file)

  # Set GDAL environment variables for HTTP requests
  Sys.setenv(GDAL_HTTP_HEADER_FILE = header_file)
  Sys.setenv(GDAL_HTTP_USERAGENT = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
  Sys.setenv(GDAL_HTTP_UNSAFESSL = "YES")  # Allow HTTPS connections

  rc_client <- ows4R::WFSClient$new(
   url = rc_url,
   serviceVersion = "1.0.0",
   logger = "INFO",
   headers = c(
     "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
     "Content-Type" = "application/xml",
     "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8",
     "Accept-Language" = "en-US,en;q=0.9",
     "Accept-Encoding" = "gzip, deflate, br",
     "Connection" = "keep-alive",
     "Upgrade-Insecure-Requests" = "1",
     "Sec-Fetch-Dest" = "document",
     "Sec-Fetch-Mode" = "navigate",
     "Sec-Fetch-Site" = "none",
     "Sec-Fetch-User" = "?1"
   )
  )
  assign("rc_client", rc_client, env =  .GlobalEnv)
  invisible(NULL)
}
