#' Analgesic potency study.
#'
#' These data are from a study (Grewal, 1952) of a method for testing analgesics in mice. The number of electric shocks until a response (a squeak) is elicited from a mouse is recorded. Then the mouse is administered an analgesic and the number of shocks until a response is recorded again. A mouse is said to be "responding" to the analgesic if the number of shocks required to elicit a response was at least four more than that required prior to the administration of the analgesic.
#'
#' @format A data frame 14 observations and four variables:
#' \describe{
#'    \item{analgesic:}{type of analgesic administered (morphine hydrochloride, amidone, phenadoxone, or pethidine hydrochloride)}
#'    \item{deposit:}{amount of insecticide in mg per 10 square cm}
#'    \item{responding:}{number of mice responding to the analgesic (see above)}
#'    \item{tested:}{number of mice tested}
#' }
#'
#' @note The study also tested mice given 200 and 400 mg/kg of phenazone to 32 and 78 mice, respectively. Of those only 25\% and approximately 33.5\%, respectively, were found to be responding to the analgesic. A higher dose of 800 mg/kg was lethal. Because of the difference in scale of the dose and the relative ineffectiveness of the analgesic, these data are not included in the data frame but could easily be added by the user. 
#'
#' @source Grewal, R. S. (1952). A method for testing analgesics in mice. \emph{British Journal of Pharmacology and Chemotherapy}, \emph{7}, 433-437. 
#' 
"analgesics"