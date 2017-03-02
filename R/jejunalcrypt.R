#' Effect of radiation on jejunal crypts in rats. 
#'
#' Data from an experiment where mice were exposed to a single dose of radiation. The mice were then sacrificed and the number of surviving jejunal crypts (intestinal glands in the small intestine) were counted.
#'
#' @format A data frame with 126 observations on two variables:
#' \describe{
#'    \item{dose:}{dose of radiation in Gy (gray)}
#'    \item{crypts:}{number of surviving jejunal crypts}
#' }
#'
#' @note A complication with these data is that the number of jejnual crypts prior to exposure to radiation is unknown and cannot be determined postmortem. The distribution of the number of surviving crypts given the dose appears to exhibit \emph{underdispersion} if a Poisson distribution is assumed.  
#'
#' @source Kim, D. K. & Taylor, J. M. G. (1994). Transform-both-sides approach for overdispersed binomial data when N is unobserved. \emph{Journal of the American Statisical Association}, \emph{89}, 833-845.
#' 
#' Faddy, M. J. & Smith, D. M. (2011). Analysis of count data with covariate dependence in both mean and variance. \emph{Journal of Applied Statistics}, \emph{38}, 2683-2694.
"jejunalcrypt"