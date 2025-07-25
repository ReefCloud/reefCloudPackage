% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lag_covariates.R
\name{lag_covariates}
\alias{lag_covariates}
\title{Lag covariates}
\usage{
lag_covariates(cov, year_range, full_cov_lookup, cov_name)
}
\arguments{
\item{cov}{Extracted disturbance layer corresponding to the monitored years}

\item{year_range}{Range of monitoring years}

\item{full_cov_lookup}{Full time series of the disturbance}

\item{cov_name}{Name of the disturbance layer}
}
\value{
A data frame with lagged covariate values
}
\description{
Compute lag values from the full time series of a disturbance layer
}
\examples{
lag_covariates(cov = cov_dhw, year_range = 2010:2022, full_cov_lookup = full_dhw, cov_name = "dhw")
}
\author{
Julie Vercelloni
}
