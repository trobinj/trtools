#' Rotifer relative density experiment.  
#'
#' These data are from a study to estimate the relative density of rotifers (a micoscopic aquatic
#' invertebrate). Batches of rotifers were put into tubes of a solution (Ficoll, a sygar polymer)
#' of known density that were then put into a centrifuge. The number of rotifers that remained in
#' suspension afterwards was then observed.
#'
#' @format A data frame with 40 observations and four variables:
#' \describe{
#'    \item{density}{density of solution}
#'    \item{species}{species of rotifer: \emph{Polyathra major} (pm) or \emph{Keratella cochlearis} (kc)}
#'    \item{y}{number of rotifers remaining in suspension}
#'    \item{total}{total number of rotifers}
#' }
#'
#' @note These data are also in the \strong{MASS} package (see \code{\link[MASS]{rotifer}}). They are
#' reshaped here for convenience to make them more amenable to regression modeling.
#'
#' @seealso \code{\link[MASS]{rotifer}}
#'
#' @source Saunders-Davies, A. P. & Pontin, R. M. (1987). A centrifugation method for measuring the
#' relative density (specific gravity) of planktonic rotifers (Rotifera), with values for the relative
#' density of \emph{Polyarthra major} (Burckhardt) and \emph{Keratella cochlearis} (Gosse).
#' \emph{Hydrobiologia}, \emph{147}, 379-381.
#'
#' Collett, D. (2003). \emph{Modelling binary data} (Second Edition). Boca Raton, FL: Chapman & Hall/CRC.
"rotifer"
