#' Create data frame with binary response variables for discrete survival analysis (experimental).
#' 
#' This is a function takes an existing data frame with a discrete time variable and coverts the time variable into a set of binary response variables for modeling discrete survival time using the binary variables to model the discrete hazard function. It can also be use to code binary responses for a sequential (continuation ratio) regression model.
#' 
#' @param data The data frame containing the time variable.
#' @param y Name of the time variable in \code{data}.
#' @param event Indicator variable for observed (i.e., not censored) events where \code{event = 1} if the event was observed at \code{y} and \code{event = 0} if the event had not yet occurred by time \code{y}. If missing then it is assumed that no observations are censored.
#' @param unit.name Variable name for observational units. 
#' @param time.name Variable name prefix for the time point of each binary response.
#' @param resp.name Variable name for the binary response variables.
#' @param open Logical for whether the maximum observed time point (\eqn{k}) should be considered as corresponding to an interval where the right endpoint is infinity so that \eqn{P(T = k|T \ge k) = 1}. It is assumed in this case that all observations of \eqn{Y = k} are effectively censored at \eqn{Y = k - 1}. This requires one less binary response variable. Default is FALSE.
#' @param reverse Reverse the binary indicator so that \eqn{P(Y_t = 1) = P(T > t|T \ge t)}. Default is FALSE. 
#' @param long Should the data be output in long-form (one binary response per row) (default is TRUE). 
#' 
#' @details Assuming survival time is integer-valued as \eqn{T = 1,2,\dots,k}, the probability of a given response can be modeled as \eqn{P(T = 1) = \lambda(1)} and \deqn{P(T = t) = \lambda(t)(1 - \lambda(j-1))(1 - \lambda(j-2))\dots(1 - \lambda(1))} for \eqn{T > 1}, where \eqn{\lambda(t) = P(T = t|T \ge t)} is the hazard function. If we define a set of binary response variables as \eqn{Y_k = 1} if \eqn{k = t} and \eqn{Y_k = 0} if \eqn{t > k}, then \eqn{P(T = 1) = P(Y_1 = 1)} and \deqn{P(T = t) = P(Y_t = 1)P(Y_{t-1} = 0)P(Y_{t-2} = 0)\dots P(Y_1 = 0)} for \eqn{T > 1}. If \eqn{Y} is censored at \eqn{T = t}, meaning that it is only known that \eqn{T > t}, then \deqn{P(T = t) = (1 - \lambda(t))(1 - \lambda(t-1))\dots (1 - \lambda(1)) = P(Y_t = 0)P(Y_{t-1} = 0) \dots P(Y_1 = 0).} Because the likelihood function for \eqn{T} is equivalent to that of the product of \eqn{T} independent binary responses, discrete survival time can be modeled as a set of binary response variables using logistic regression or other models for independent binary responses to model the hazard function. 
#' 
#' A related case is the continuation ratio or sequential regression model for ordinal response variables. There \eqn{T} represents an ordinal response (not necessarily time) and typically one models \eqn{P(T > t|T \ge t)} and assumes that a response will necessarily be in the last category if it is not in the previous category meaning that \eqn{P(T = k|T \ge k) = 1} if \eqn{k} is the highest "value" of \eqn{T}. This is effectively equivalent to a discrete survival model for the probability that the event does not occur at time \eqn{T} given that it has not yet occurred, and all observations of \eqn{Y = k} are censored at \eqn{Y = k - 1}. This can be achieved by using the \code{open = TRUE} and \code{reverse = TRUE} (see the example below). 
#' 
#' @importFrom tidyr gather
#' @importFrom dplyr arrange
#' @examples 
#' # setup for discrete survival model where the first five times are right-censored
#' d <- data.frame(time = rep(1:5, 2), x = rnorm(10), status = rep(0:1, each = 5))
#' dsurvbin(d, "time", "status")
#' # setup for a continuation ratio (sequential regression) model
#' d <- data.frame(time = rep(1:5, 2), x = rnorm(10))
#' dsurvbin(d, "time", open = TRUE, reverse = TRUE)
#' @export
dsurvbin <- function (data, y, event, unit.name = "unit", time.name = "t", 
  resp.name = "y", open = FALSE, reverse = FALSE, long = TRUE) 
{
  if (length(intersect(names(data), unit.name)) > 0) {
    stop("variable name conflict, change unit.name")
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
  if (!long) {
    names(z) <- paste(time.name, 1:k, sep = "")
    if (length(intersect(names(data), names(z))) > 0) {
      stop(paste("variable name conflict, change time.name"))
    }            
    if (reverse) {
      z <- 1 - z
    }
    return(cbind(data, z))
  }
  names(z) <- as.character(1:k)
  out <- cbind(data, z)
  out[[unit.name]] <- 1:nrow(out)
  out <- tidyr::gather(out, key = !!time.name, value = !!resp.name, names(z))
  out <- dplyr::arrange(out, rep(1:nrow(data), k))
  out <- out[!is.na(out[[resp.name]]), ]
  if (reverse) 
    out[[resp.name]] <- 1 - out[[resp.name]]
  return(out)
}
