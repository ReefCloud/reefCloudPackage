% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_validation.R
\name{tidy_validation}
\alias{tidy_validation}
\title{Tidy validation results}
\usage{
tidy_validation(result, rules)
}
\arguments{
\item{result}{The result object returned by `validate()`.}

\item{rules}{The validation rules used in the check.}
}
\value{
A tidy data frame with rule descriptions and validation outcomes.
}
\description{
Summarise and reformat the results of a data validation check.
}
\examples{
rules <- validator(!is.na(SITE_ID), COVER >= 0)
result <- confront(benthic_data, rules)
tidy_validation(result, rules)
}
\author{
Murray Logan
}
