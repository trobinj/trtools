#' Stratified random sample of daphnia counts.
#'
#' These data are from a stratified random sample from three layers of a
#' lake: epilimnion, thermocline, and hypolimnion. The volumes of these
#' layers are 100kL, 200kL, and 400kL respectively, so that the sampling
#' fractions are 1/7, 2/7, and 4/7, respectively. The sampling units are
#' one liter containers of water, and the target variable is daphnia per
#' liter.
#'
#' @format A data frame with 45 observations and two variables:
#' \describe{
#'    \item{stratum}{layer from which the water sample was taken}
#'    \item{count}{number of daphnia in the liter of water}
#' }
#'
#' @source Barrett, J. P. & Nutt, M. E. (1979). \emph{Survey sampling in the environmental
#' sciences: A computer approach}. Wentworth, NH: COMPress, Inc.
#'
#' Gregoire, T. G. & Valentine, H. T. (2007). \emph{Sampling strategies for natural resources
#' and the environment}. Boca Raton, FL: Chapman & Hall/CRC.
"daphnia"
