#' Pulse rates before and after exercise. 
#'
#' Data from a classroom experiment investigating the effect of exercise (running in place) on pulse rates. Assignment to the experimental treatment (ran) was randomized.
#'
#' @format A data frame with 110 observations and 11 variables:
#' \describe{
#'    \item{height:}{height (cm)}
#'    \item{weight:}{weight (kg)}
#'    \item{age:}{age (years)}
#'    \item{gender:}{gender (male or female)}
#'    \item{smokes:}{smoking (yes or no)}
#'    \item{alcohol:}{alcohol use (yes or no)}
#'    \item{exercise:}{frequency of exercise (high, moderate, or low)}
#'    \item{treatment:}{activity between pulse measurements (ran or sat)}
#'    \item{pulse1:}{first pulse measurement (per minute)}
#'    \item{pulse2:}{second pulse measurement (per minute)}
#'    \item{year:}{year of class}
#' }
#'
#' @note One issue with these data noted in the source (see below) is that there is some concern of non-compliance. Some students assigned to the running condition may have elected to not run instead.
#'
#' @source http://www.statsci.org/data/oz/ms212.html
"pulse"