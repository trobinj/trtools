#' Survey of prevalence of psychiatric disorders in Gulf War veterans. 
#'
#' Two-phase stratified random sampling design for the prevalence of psychiatric disorders in Gulf War veterans. In the first phase a random sample of veterans from a population of 53,462 were administered the SF-36 questionnaire. Respondents were classified as either disabled or not disabled. The second phase used a stratified random sampling design from the first sample. The sampled veterans were then assessed by psychiatrists to determine if they had an alcohol, sleep, or other psychiatric disorder.
#'
#' @format A data frame 3453 observations and four variables:
#' \describe{
#'    \item{disabled:}{classification of respondent based on SF-36 questionnaire (disabled or not-disabled)}
#'    \item{alcohol:}{alcohol related disorder (yes or no)}
#'    \item{sleep:}{sleep related disorder (yes or no)}
#'    \item{psych:}{psychiatric related disorder (yes or no)}
#' }
#' 
#' @note Becauses the data are reconstructed based on marginal counts, the co-occurrence of the three disorders are not necessarily correct and should not be used. The data are orginally from Ismail et al. (2002) and are featured in Lohr (2010).
#'
#' @source Ismail et al. (2002). The mental health of UK gulf war veterans: Phase 2 of a two phase cohort study. \emph{British Medical Journal}, \emph{325}, 576--579.
#' 
#' Lohr, S. L. (2010). \emph{Sampling: Design and analysis} (2nd edition). Boston: Brooks/Cole.
#' 
"ismail"