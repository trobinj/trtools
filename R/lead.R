#' Lead absorption in children.
#' 
#' Data from a study in Oklahoma in 1978 that used a matched-pairs design to compare the blood lead levels of children (12 to 83 months) of employees in a lead-related industry to that of children of parents who were not in a lead-related industry (Morton et al., 1982). Children were matched based on age, neighborhood, and nature of the residence (e.g., same apartment complex, proximity to the street). Blood lead levels are in micrograms per dl. 
#'
#' @format A data frame of 33 observations and four variables.
#' \describe{
#'    \item{level:}{blood lead level of exposed children (i.e., children with a parent in a lead-related industry)}
#'    \item{control:}{blood lead level of child with a parent not in a lead-related industry}
#'    \item{hygiene:}{parent compliance with hygienic practice to reduce child exposure to lead dust}
#'    \item{exposure:}{parent level of exposure to lead}
#' }
#'
#' @note The blood lead levels of the control children are matched by value with those of the children of parents in a lead-related industry, but not necessarily with the values of the hygiene or exposure covariates This is because the values of the covariates were obtained from tables in Morton et al. (1982) that showed only the data for the children of parents in a lead-related industry. Furthermore there was one observation (the observation with a blood lead level of 29) in which no control child could be identified in the tables. Finally it was noted that the observation with a blood lead level of 39 and a exposure of medium may be an error based on the personal observation of the employee by the interviewer.
#' 
#' @source Morton, D. E., Saah, A. J., Silberg, S. L., Owens, W. L., Roberts, M. A., & Saah, M. D. (1982). Lead absorption in children of employees in a lead-related industry. \emph{American Journal of Epidemiology}, \emph{115}, 549-555. 
#' 
"lead"

