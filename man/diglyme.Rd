% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diglyme.R
\docType{data}
\name{diglyme}
\alias{diglyme}
\title{Toxicity of diethylene glycol dimethyl ether.}
\format{
A data frame of 5 observations on four variables.
\describe{
   \item{concentration:}{concentration of diethylene glycol dimethyl ether (mg/kg per day)}
   \item{dead:}{number of dead fetuses}
   \item{malformation:}{number of malformed fetuses}
   \item{normal:}{number of normal fetuses}
}
}
\source{
Price, C. J., Kimmel, C. A., George, J. D., & Marr, M. C. (1987). The developmental toxicity study of diethylene glycol dimethyl ether in mice. \emph{Fundamental and Applied Toxicology}, \emph{8}, 115--126. The data were also featured in Categorical Data Analysis by Agresti.
}
\usage{
diglyme
}
\description{
Data from an experiment where pregnant mice were exposed to varying concentrations of diethylene glycol dimethyl ether (diglyme) for ten days. Two days later the mice fetuses were examined and classified as dead, malformation, or normal.
}
\note{
Agresti notes that the observations are not necessarily independent. Some dependencies may exist among observations of fetuses from the same litter.
}
\keyword{datasets}
