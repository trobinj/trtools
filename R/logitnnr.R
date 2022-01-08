#' Logit link with a natural non-response parameter.
#' 
#' Link function for logistic regression models with a (fixed/known) natural non-response parameter. 
#' 
#' @param nnrp Natural non-response parameter in [0,1). 
#' 
#' @details The logit link function with a natural non-response parameter is defined as \eqn{log[E(Y) / (1 - \pi - E(Y))]} and inverse link function \eqn{E(Y) = (1 - \pi)/(1 + exp(-\eta))}, where \eqn{0 \le \pi < 1} is the natural non-response parameter. Note that the usual logit link function is a special case where \eqn{\pi = 0}. The link function implies an upper asymptote of \eqn{1 - \pi} for \eqn{E(Y)} as \eqn{\eta} approaches infinity.
#' 
#' @note User-specified starting values for the regression parameters may be required in cases where \eqn{\pi} is substantially larger than zero. One strategy is to obtain these from a model with a smaller value of \eqn{\pi}
#' 
#' @seealso \code{\link{logitnr}}
#' 
#' @importFrom stats dlogis plogis
#' @export
logitnnr <- function(nnrp = 0) {
  linkfun <- function(mu) {
    log(mu / (1 - nnrp - mu))
  }
  linkinv <- function(eta) {
    (1 - nnrp) * plogis(eta)
  } 
  mu.eta <- function(eta) {
    (1 - nnrp) * dlogis(eta)
  }
  valideta <- function(eta) TRUE
  structure(list(linkfun = linkfun, linkinv = linkinv, mu.eta = mu.eta,
    valideta = valideta, name = "logitnnr"), class = "link-glm")
}
