% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rm_obs_outside.R
\name{rm_obs_outside}
\alias{rm_obs_outside}
\title{Remove observations outside spatial polygons}
\usage{
rm_obs_outside(data.grp.tier, HexPred_reefid2, i, N)
}
\arguments{
\item{data.grp.tier}{Data frame filtered by tier with coordinates (`LONGITUDE` and `LATITUDE` columns).}

\item{HexPred_reefid2}{Spatial polygons (`sf` object) defining Tier5 covariate areas.}
}
\value{
Filtered data frame with observations inside polygons (no geometry column).
}
\description{
Remove observations outside Tier5 cells
}
\details{
Filters observations to retain only those falling inside given spatial polygons.
}
\examples{
\dontrun{
library(sf)
# Example data frame of points
points_df <- data.frame(
  LONGITUDE = c(153.0, 153.1, 153.2),
  LATITUDE = c(-27.5, -27.6, -27.7),
  value = c(10, 20, 30)
)

# Example polygon (rectangle)
polygon_sf <- st_as_sf(
  data.frame(id = 1),
  wkt = "POLYGON((152.9 -27.8, 153.3 -27.8, 153.3 -27.4, 152.9 -27.4, 152.9 -27.8))",
  crs = 4326
)

filtered_points <- rm_obs_outside(points_df, polygon_sf)
}
}
\author{
Julie Vercelloni
}
