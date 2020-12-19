#' Stability of a drug stored in bottle or blister packaging.  
#'
#' Data from an experiment where 300 mg tablets from several batches were stored in either high-density polyethylene bottle or blister packages. The stability of the drugs in terms of the percent of claimed amount was assayed at several time periods.
#'
#' @format A data frame with 300 observations and five variables:
#' \describe{
#'    \item{assay:}{assay result (percent of claim)}
#'    \item{package:}{packaging type (bottle or blister)}
#'    \item{batch:}{drug batch}
#'    \item{months:}{time in storage}
#' }
#'
#' @note Batches are nested within package. The data are from Shao and Chow (1994).
#'
#' @source Shao, J. & Chow, S. C. (1994). Statistical inference in stability analysis. \emph{Biometrics}, \emph{50(3)}, 753-763.
#' 
"stability"