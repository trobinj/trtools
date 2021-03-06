% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grasshopper.R
\docType{data}
\name{grasshopper}
\alias{grasshopper}
\title{Assay of grasshopper insecticide and synergist.}
\format{
A data frame with 15 observations and four variables:
\describe{
   \item{y:}{number of grasshoppers killed}
   \item{m:}{total number of grasshoppers}
   \item{insecticide:}{dose of insecticide (units unknown)}
   \item{synergist:}{dose of synergist (units unknown)}
}
}
\source{
McCullagh, P. & Nelder, J. A. (1989). \emph{Generalized linear models} (2nd Edition). Boca Raton: Chapman & Hall/CRC.
}
\usage{
grasshopper
}
\description{
These data are from an assay on grasshoppers (\emph{Melanopus sanguinipes}) exposed to varying does of an insecticide (carbofuran) and a synergist (piperonyl butoxide). The synergist enhances the effect of the insecticide.
}
\note{
McCullagh and Nelder (1989) used these data to demonstrate nonlinear logistic regression models with transformations of insecticide and synergist with unknown parameters in the transformations.
}
\keyword{datasets}
