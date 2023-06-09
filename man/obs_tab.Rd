% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/obs_tab.R
\name{obs_tab}
\alias{obs_tab}
\title{Data table for supporting data}
\usage{
obs_tab(strat)
}
\arguments{
\item{strat}{Level of stratification - 'No', 'OmradeNavn' or 'Year'}
}
\description{
Generates a data table with an overview of the Line ID's and
the stratification label ('År' or 'Områdenavn') for each observation.
}
\examples{
obs_tab(strat = LEVEL)
}
\keyword{lineID}
\keyword{stratification}
\keyword{table}
