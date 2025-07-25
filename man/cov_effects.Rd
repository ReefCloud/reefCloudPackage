% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cov_effects.R
\name{cov_effects}
\alias{cov_effects}
\title{Scale Up Predictions from Model Outputs}
\usage{
cov_effects()
}
\arguments{
\item{whichModel}{Character string indicating the model type (e.g., "type5").}
}
\value{
This function has no return value. It writes summarised prediction CSV files to disk.
}
\description{
This function reads model outputs (either type5 or other specified models), processes them
to scale up predictions across different tiers, and saves the summarised results.
}
\examples{
scale_up_pred("type5")
}
