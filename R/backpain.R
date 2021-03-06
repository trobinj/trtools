#' Risk of driving on herniated discs.
#'
#' These data are from a 1:1 matched case control study to investigate the risk of driving on lower back pain caused by acute herniated lumbar intervertebral discs. A total of 217 cases were individuals between the ages of 20 and 64 living in New Haven, Connecticut. For each case a control was selected from patients admitted to the same hospital as the case, and matched with the case based on sex and age (within 10 years). 
#'
#' @format A data frame with 434 observations and four variables.
#' \describe{
#'    \item{group:}{case or control}
#'    \item{driver:}{whether or not the individual is a driver (no or yes)}
#'    \item{residence:}{residence of individual (city or suburban)}
#'    \item{set:}{integer-valued identifier for a matched pair of individuals}
#' }
#'
#' @note These data are featured in Collett (2003). The study is described in Kelsey and Hardy (1975) and Holford, White, and Kelsey (1978).
#'
#' @source Holford, T. R., White, C., & Kelsey, J. L. (1978). Multivariate analysis for matched case-control studies. \emph{American Journal of Epidemiology}, \emph{107}, 245-256.
#' 
#' Kelsey, J. L. & Hardy, H. J. (1975). Driving of motor vehicles as a risk factor for acute herniated lumbar intervertebral disc. \emph{American Journal of Epidemiology}, \emph{102}, 63-73. 
#' 
#' Collett, D. (2003). \emph{Modelling binary data} (2nd Edition). Boca Raton: Chapman & Hall/CRC. 
"backpain"