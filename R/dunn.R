#' Survey of prevalence of psychiatric disorders. 
#'
#' Two-phase stratified random sampling design for the prevalence of psychiatric disorders. Participants sampled in the first phase were classified as low, medium, or high based on their score on the General Health Questionnaire (GHQ). These were then used as strata for a stratified random sampling design in the second phase. Sampled partipants in the second phase were classified as having at least one psychiatric disorder (case) or having no psychiatric disorder (non-case) using the Composite International Diagnostic Interview (CIDI).
#'
#' @format A data frame 1558 observations and two variables:
#' \describe{
#'    \item{ghq:}{classification based on the GHQ score (low, medium, or high)}
#'    \item{cidi:}{classification based on the CIDI (case, non-case)}
#' }
#' 
#' @note The data are originally from Dunn et al. (1999) and are featured in Lohr (2010). The data are fictional. 
#'
#' @source Dunn, G., Pickles, A., Tansella, M., and Vazquez-Barquero, J. (1999). Two-phase epidemiological surveys in psychiatric research. \emph{British Journal of Psychiatry}, \emph{174}, 95--100. 
#' 
#' Lohr, S. L. (2010). \emph{Sampling: Design and analysis} (2nd edition). Boston: Brooks/Cole.
#' 
"dunn"