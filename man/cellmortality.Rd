% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cellmortality.R
\docType{data}
\name{cellmortality}
\alias{cellmortality}
\title{Mortality of cancer cells.}
\format{
A data frame with 27 observations and 3 variables:
\describe{
   \item{occasion:}{occasion on which the cells were observed (1-9)}
   \item{dish:}{dish of 400 cells (1-27)}
   \item{survived:}{number of cells that survived}
}
}
\source{
Schall, R. (1991). Estimation in generalized linear models with random effects. \emph{Biometrika}, \emph{78}, 719--727. The data were downloaded from http://www.statsci.org/data/general/radiatio.html.
}
\usage{
cellmortality
}
\description{
Data from an experiment investigating the mortality of cancer cells after exposure to radiation. These data are from the control condition where the cells were not irradiated so as to estimate natural mortality. On each of nine occasions, three dishes of 400 cells each were observed.
}
\keyword{datasets}
