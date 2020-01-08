#' Survey of households for televisions.
#'
#' Survey carried out in Des Moines, Iowa, in 1951. Sampling units were clusters of households. The number of households with televisions was recorded in each selected sampling unit. The total number of households in the sampling frame was 56296, and the total number of sampling units in the frame was 9460.  
#' 
#' @format A data frame of 132 observations and two variables:
#' \describe{
#'    \item{households:}{number of households in the sampling unit}
#'    \item{tv:}{number of households with televisions in the sampling unit}
#' }
#' 
#' @note The data are from Jessen (1978). The 82nd unit was originally reported as having 44 households, which is likely an error. It was changed to 4 here, which matches the summary statistics reported in the book for the households variable. The summary statistics for the tv variable, however, do not match those reported in the book, and the source of the discrepancy is not clear. 
#' 
#' @source Jessen, R. J. (1978). \emph{Statistical survey techniques}. Wiley.
#' 
"televisions"