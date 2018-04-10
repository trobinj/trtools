#' Fecundability and smoking.
#'
#' Data from an observational study that recorded the number of menstrual cycles required to achieve pregnancy for smoking and non-smoking women. 
#'
#' @format A data frame of 586 observations and two variables:
#' \describe{
#'    \item{cycles:}{number of menstrual cycles until pregnancy}
#'    \item{mother:}{smoking status of mother}
#' }
#'
#' @note Note that the number of cycles is right-censored at 13 so that all observationrecorded as 13 are actually 13+. These data were featured in Pawitan (2013) and Weinberg and Gladen (1986). They are a subset of the data collected in a study described by Baird and Wilcox (1985). 
#'
#' @source Baird, D. D. & Wilcox, A. J. (1985). Cigarette smoking associated with delayed conception. \emph{Journal of the American Medical Association}, \emph{253}, 2979-2983.
#' 
#' Pawitan, Y. (2013). \emph{In all likelihood: Statistical modeling and inference using likelihood}. Oxford University Press. 
#' 
#' Weinberg, C. R. & Gladen, B. C. (1986). The beta-geometric distribution applied to comparative fecudability studies. \emph{Biometrics}, \emph{42}, 547-560.
"cycles"