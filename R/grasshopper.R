#' Assay of grasshopper insecticide and synergist.   
#'
#' These data are from an assay on grasshoppers (\emph{Melanopus sanguinipes}) exposed to varying does of an insecticide (carbofuran) and a synergist (piperonyl butoxide). The synergist enhances the effect of the insecticide.
#'
#' @format A data frame with 15 observations and four variables:
#' \describe{
#'    \item{y:}{number of grasshoppers killed}
#'    \item{m:}{total number of grasshoppers}
#'    \item{insecticide:}{dose of insecticide (units unknown)}
#'    \item{synergist:}{dose of synergist (units unknown)}
#' }
#'
#' @note McCullagh and Nelder (1989) used these data to demonstrate nonlinear logistic regression models with transformations of insecticide and synergist with unknown parameters in the transformations. 
#' 
#' @source McCullagh, P. & Nelder, J. A. (1989). \emph{Generalized linear models} (2nd Edition). Boca Raton: Chapman & Hall/CRC.
"grasshopper"