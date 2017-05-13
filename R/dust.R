#' Rodent tissue radioactiviation from in vitro exposure to asbestos dust.
#'
#' Piergorsch (1992) describes the data as "a laboratory experiment where rodent alveolar/bronchiolar tissue was exposed in vitro to asbestos dust, to examine the inflammatory effects of asbestos exposure to mammalian lung tissue. The alveolar/bronchiolar cells were pretreated with an assumed-benign radioactive marker that is released after asbestos dust exposure, indicating whether any cell "activated," that is, generated an inflammatory response. Of interest was the effect of time (as hours after exposure, t) on the inflammatory response; additional explanatory variables of interest included cell type (epithelial or interstitial) and original cell location (terminal bronchial airway, alveolar duct, or bifurcation duct), which were arranged in a 2 x 3 factorial design." He notes that the number of cells activiting on a given plate could not be observed. It could only be observed if there was any radioactiviation on a given plate. 
#'
#' @format A data frame of 24 observations and five variables.
#' \describe{
#'    \item{time:}{time of observation (hours)}
#'    \item{location:}{cell location of T (terminal bronchial airway), A (alveolar duct), or B (bifurcation duct)}
#'    \item{type:}{cell type of E (epithelial) or N (interstitial)}
#'    \item{examined:}{number of plates examined}
#'    \item{responding:}{number of plates exhibiting radiactiviation} 
#' }
#'
#' @note Piegorsch (1992) uses these data to motivate a generalized linear model with a complementary log-log link function since the response may have (approximately) truncated Poisson distribution, although he found that the logit link function may provide a better fit to the data. 
#' 
#' Piegorsch, W. W. (1992). Complementary log-log regression for generalized linear models. \emph{The American Statistician}, \emph{46}, 94-99.
#' 
"dust"

