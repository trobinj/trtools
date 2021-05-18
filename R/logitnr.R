#' Logit link with a natural response parameter.
#' 
#' Link function for logistic regression models with a (fixed/known) natural response parameter.
#' 
#' @param nrp Natural response parameter in [0,1). 
#' 
#' @details The logit link function with a natural response parameter is defined as \eqn{log((E(Y) - \pi)/(1 - E(Y))} and inverse link function \eqn{E(Y) = \pi + (1 - \pi)/(1 + exp(-\eta))}, where \eqn{0 \le \pi < 1} is the natural response parameter. Note that the usual logit link function is a special case where \eqn{\pi = 0}. The link function implies a lower asymptote of \eqn{\pi} for \eqn{E(Y)} as \eqn{\eta} approaches negative infinity. 
#' 
#' @note User-specified starting values for the regression parameters may be required in cases where \eqn{\pi} is substantially larger than zero. One strategy is to obtain these from a model with a smaller value \eqn{\pi}. 
#' 
#' @seealso \code{\link{logitnnr}}
#' 
#' @examples 
#' # obtain starting values with the natural response parameter set to 0
#' tmp <- glm(cbind(total-y, y) ~ density + species, 
#'  family = binomial(link = logitnr(0)), data = rotifer)
#' # use starting values with a fixed natural response parameter of 0.1
#' glm(cbind(total-y, y) ~ density + species, 
#'  family = binomial(link = logitnr(0.1)), data = rotifer, start = coef(tmp))
#' @importFrom stats dlogis plogis
#' @export
logitnr <- function(nrp = 0) {
  linkfun <- function(y) {
    log((y - nrp) / (1 - y))
  }
  linkinv <- function(eta) {
    nrp + (1 - nrp) * plogis(eta)
  }
  mu.eta <- function(eta) {
    -(nrp - 1) * dlogis(eta)
  }
  valideta <- function(eta) TRUE
  structure(list(linkfun = linkfun, linkinv = linkinv, mu.eta = mu.eta, 
    valideta = valideta, name = "logitnr"), class = "link-glm")
}