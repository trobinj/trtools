#' Survey of radon concentration in homes in Minnesota.
#'
#' Data from a survey of radon levels in homes in Minnesota. The survey used a
#' stratified random sampling design with counties as strata. See Nolan and
#' Speed (2009) for details about the sampling design.
#'
#' @format A data frame with 1003 observations and four variables:
#' \describe{
#'    \item{county}{county name}
#'    \item{n}{number of homes in the county and in the sample}
#'    \item{N}{number of homes in the county}
#'    \item{radon}{radon concentration in pCi/L}
#' }
#'
#' @note As noted by Nolan and Speed (2009) there was some non-response but
#' no information is given for non-responders.
#'
#' @source Nolan, D. & Speed, T. P. (2009). \emph{Stat Labs: Mathematical statistics through applications}. New York: Springer.
#'
#' These data were obtained from \url{http://www.stat.ncsu.edu/people/boos/courses/st715/schedule.715.html}.
"radonminn"
