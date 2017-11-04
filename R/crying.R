#' Effect of stimulation on crying in newborn infants.
#'
#' Data from an experiment that investigated the effect of stimulation (rocking) on crying in newborn infants. Each day of 18 days one infant in a ward was selected at random and rocked for 30 minutes. All babies in the ward were then observed for the next 30 minutes. 
#'
#' @format A data frame 32 observations and four variables:
#' \describe{
#'    \item{day:}{day of observation (1-18)}
#'    \item{treatment:}{whether or not baby was rocked}
#'    \item{babies:}{number of babies observed}
#'    \item{crying:}{number of babies crying}
#' }
#'
#' @note There are a a few potential complications in the design. As noted by the authors, on the 4th day the temperature in the ward was very low until near the end of the observation period. Also it is not known if the same babies were in the ward on more than one day of observation (which were not necessarily adjacent). Finally babies that were already crying before the selection of the baby to be rocked were never assigned to the rocking condition. 
#'
#' @source Gordon, T. & Foss, B. M. (1965). The role of stimulation in the delay of onset of crying in the newborn infant. \emph{Quarterly Journal of Experimental Psychology}, \emph{18(1)}, 79--81.
#' 
"crying"