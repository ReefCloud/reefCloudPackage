% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_contrasts.R
\name{make_contrasts}
\alias{make_contrasts}
\title{Compile Tier-Level Fold Changes}
\usage{
make_contrasts(pred_tierIndex, tier_col)
}
\arguments{
\item{pred_tierIndex}{Data frame with posterior predictions including year, tier, and cover values.}

\item{tier_col}{Character string naming the tier column (e.g., "Tier4").}
}
\value{
List of tibbles with fold changes and probabilities for each tier level.
}
\description{
Computes year-on-year fold changes and directional probabilities from posterior draws by tier.
}
\author{
Julie Vercelloni
}
