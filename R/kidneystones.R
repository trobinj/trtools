#' Comparison of treatments for kidney stones. 
#'
#' Data from an observational study that compared the effectiveness of several treatments for kidney stones. 
#'
#' @format A data frame with 12 observations and four variables:
#' \describe{
#'    \item{size:}{kidney stone size (less or greater than or equal to 2cm)}
#'    \item{treatment:}{treatment method as a combination of nephrolithotomy and pyelolithotomy (nephr/pyelo), pyelolithotomy (pyelo), ureterolithotomy (uretero), percutaneous nephrolithotomy (perc/nephro), ESWL (extracorporeal shock wave lithotripsy), or percutaneous nephrolithotomy with extracorporeal shock wave lithotripsy (perc/nephro/ESWL)}
#'    \item{success:}{number of successful treatments}
#'    \item{total:}{total number of treatments}
#' }
#'
#' @note Julious and Mullee (1994) used these data from Charig et al. (1986) to illustrate Simpson's paradox by comparing percutaneous nephrolithotomy with the open surgical methods (i.e., nephrolithotomy and pyelolithotomy, pyelolithotomy, and ureterolithotomy) grouped into a single treatment condition. The paradox is observed when the methods are compared with and without controlling for stone size. This example is also discussed at \url{https://en.wikipedia.org/wiki/Simpson's_paradox}. 
#'
#' @source Charig, C. R., Webb, D. R., Payne, S. R., & Wickham, J. E. A. (1986). Comparison of treatment of renal calculi by open surgery, percutaneous nephrolithotomy, and extracorporeal shockwave lithotripsy. \emph{British Medical Journal}, \emph{292}, 879-882.
#' 
#' Julious, S. A. & Mullee, M. A. (1994). Confounding and Simpson's paradox. \emph{British Medical Journal}, 1480-1481.
"kidneystones"