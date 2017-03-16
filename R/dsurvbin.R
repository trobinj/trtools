#' Create data frame with binary response variables for discrete survival analysis (experimental).
#' 
#' This is a function takes an existing data frame with a discrete time variable and coverts the time variable into a set of binary response variables for modeling discrete survival time. 
#' 
#' @param data: The data frame containing the time variable.
#' @param y: Name of the time variable in \code{data}.
#' @param event: Indicator variable for observed (i.e., not censored) events where \code{event = 1} if the event was observed at \code{y} and \code{event = 0} if the event had not yet occurred by time \code{y}. If missing then it is assumed that no observations are censored.
#' @param case.name: Variable name for cases of binary responses (i.e., all binary responses corresponding to a single survival time). 
#' @param time.name: Variable name prefix for the time point of each binary response.
#' @param resp.name: Variable name for the binary response variables.
#' @param open: Logical for whether the observed time point should be considered as corresponding to an open time interval where the right endpoint is infinity (effectively equivalent to assuming that the maximum observed value of \code{y} is right-censored). Default is FALSE.
#' @param reverse: Reverse the binary indicator so that \eqn{P(Y_t = 1) = P(T > t|T \ge t)}. Default is FALSE. 
#' 
#' @details Assuming survival time is integer-valued as \eqn{T = 1,2,\dots,k}, the probability of a given response can be modeled as \eqn{P(T = 1) = 1 - \lambda(1)} and \deqn{P(T = t) = \lambda(t)(1 - \lambda(j-1))(1 - \lambda(j-2))\dots(1 - \lambda(1))} for \eqn{T > 1}, where \eqn{\lambda(t) = P(T = t|T \ge t)} is the hazard function. If we define a set of binary response variables as \eqn{Y_k = 1} if \eqn{k = t} and \eqn{Y_k = 0} if \eqn{t > k}, then \eqn{P(T = 1) = P(Y_1 = 1)} and \deqn{P(T = t) = P(Y_t = 1)P(Y_{t-1} = 0)P(Y_{t-2} = 0)\dots P(Y_1 = 0)} for \eqn{T > 1}. Thus the likelihood function for \eqn{T} is equivalent to that of the product of \eqn{T} independent binary responses, and so discrete survival time can be modeled as a set of binary response variables using logistic regression or other models for independent binary responses. 
#' 
#' @importFrom reshape2 melt
#' @importFrom dplyr arrange
#' @export
dsurvbin <- function(data, y, event, case.name = "case", time.name = "t", resp.name = "y", 
  open = FALSE, reverse = FALSE) {
  if (length(intersect(names(data), case.name)) > 0) {
    stop("variable name conflict, change case.name")
  }
  if (length(intersect(names(data), time.name)) > 0) {
    stop("variable name conflict, change time.name")
  }
  if (length(intersect(names(data), resp.name)) > 0) {
    stop("variable name conflict, change resp.name")
  }
  z <- data[[y]]
  if (open) {
    k <- max(z) - 1
  }
  else {
    k <- max(z)
  }
  z <- data.frame(outer(z, 1:k, function(z, u) ifelse(z == u, 1, ifelse(z > u, 0, NA))))
  if (!missing(event)) {
    z <- sweep(z, 1, data[[event]], "*")
  }
  names(z) <- paste(time.name, 1:k, sep = "")
  if (length(intersect(names(data), names(z))) > 0) {
    stop(paste("variable name conflict, change time.name"))
  }
  out <- cbind(data, z)
  out[[case.name]] <- 1:nrow(out)
  out <- reshape2::melt(out, measure.var = names(z), variable.name = time.name, value.name = resp.name)
  out <- dplyr::arrange(out, rep(1:nrow(data), k))
  out <- out[!is.na(out[[resp.name]]),]
  if (reverse) out[[resp.name]] <- 1 - out[[resp.name]]
  return(out)
}
