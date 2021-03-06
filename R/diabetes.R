#' Survival of diabetic patients. 
#'
#' Grouped survival data for diabetic men by age and insulin dependence. 
#'
#' @format A data frame with four observations and four variables.
#' \describe{
#'    \item{age:}{40 or younger, or older than 40}
#'    \item{insulin:}{dependence on insulin (yes or no)}
#'    \item{dead:}{number of deceased individuals}
#'    \item{censored:}{number of individuals with alive or right-censored survival times}
#' }
#'
#' @note Julious and Mullee (1994) used these data from the Poole diabetic cohort to illustrate Simpson's paradox which occurs when the relationship between insulin dependence and survival is examined with and without controlling for age. The data are described by Gatling, Mullee, and Hill (1988) although they do not discuss survival. The time point at which survival time was dichotomized is not given. 
#'
#' @source Gatling, W., Mullee, M., & Hill, R. (1988). General characteristics of a community-based diabetic population. \emph{Practical diabetes}, \emph{5}, 104-107.
#' 
#' Julious, S. A. & Mullee, M. A. (1994). Confounding and Simpson's paradox. \emph{British Medical Journal}, 1480-1481.
"diabetes"