#' Effect of antibotics on acute otitis media in children. 
#'
#' Data from a randomized clinical trial comparing two antibiotics, amoxicillin and cefaclor, for treating acute otitis media (AOM) in children. Children with either one (unilateral) or two (bilateral) ears affected by AOM were treated with either amoxicillin or cefaclor and then observed 14 days later.  
#'
#' @format A data frame with 30 observations and five variables:
#' \describe{
#'    \item{age}{age group of child (less than two, between two and five, and older than five)}
#'    \item{treatment}{antibiotic treatment (amoxicillin or cefaclor)}
#'    \item{clear}{number of ears clear of AOM 14 days after treatment}
#'    \item{affected}{number of ears affected by AOM before treatment (unilateral or bilateral)}
#'    \item{count}{number of children}
#' }
#'
#' @note Details concerning the source of these data can be found in Mandel et al. (1982). These data are also featured in Rosner (1989).
#'
#' @source Rosner, B. (1989). Multivariate methods for clustered binary data with more than one level of nesting. \emph{Journal of the American Statistical Association}, \emph{84}, 373-380.
#' 
#' Mandel, E., Bluestone, C. D., Rockette, H. E., Blatter, M. M., Reisinger, K. S., Wucher, F. P., & Harper, J. (1982). Duration of antibiotic treatment for acute otitis media. \emph{Pediatric Infectious Diseases}, \emph{1}, 310-316.
"otitis"