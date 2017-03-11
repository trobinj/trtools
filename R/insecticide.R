#' Flour beetle response to three insecticides.
#'
#' Hewlett and Plackett (1950) reported an experiment that exposed flour beetles (\emph{Tribolium castaneum}) to one of three insecticides: dichlorodiphenyltrichloroethane (DDT), gamma-benzene hexachloride (g-BHC), and a mixture of both DDT and g-BHC. Batches of beetles were exposed to varying deposits of spray on a surface in varying amounts of mg per 10 square centimeters using a dilution of the insecticide. Batches of beetles was randomly assigned to an insecticide and a deposit amount so that there was one batch per combination of insecticide and deposit amount. The number of beetles that died after a specified amount of time was recorded for each batch.
#'
#' @format A data frame 18 observations and four variables:
#' \describe{
#'    \item{insecticide:}{insecticide to which the beetles were exposed (DDT, g-BHC, or both)}
#'    \item{deposit:}{amount of insecticide in mg per 10 square cm}
#'    \item{deaths:}{number of beetles in the batch that died}
#'    \item{total:}{total number of beetles in batch}
#' }
#'
#' @note: The data are also featured in Collett (2003). 
#' 
#' @source Collett, D. (2003). \emph{Modelling binary data} (2nd Edition). Boca Raton: Chapman & Hall/CRC. 
#' 
#' Hewlett, P. S. & Plackett, R. L. (1950). Statistical aspects of the independent joint action of poisons, particularly insecticides. II. Examination of data for agreement with the hypothesis. \emph{Annals of Applied Biology}, \emph{37}, 527-552.
#' 
"insecticide"