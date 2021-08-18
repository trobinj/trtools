#' Logit link accounting for classification error.
#' 
#' Link function for a logistic regression model where the response variable is subject to classification error with known sensitivity and specificity.
#' 
#' @param sens Sensitivity (i.e., the probability of a positive response given that the true classification is positive).
#' @param spec Specificity (i.e., the probability of a negative response given that the true classification is negative).
#' 
#' @details The inverse link function is \eqn{a/(1 + exp(-\eta)) + (1-b)/(1 + exp(\eta))} where \eqn{a} and \eqn{b} are the sensitivity and specificity, respectively. Note that this is essentially a mixture model. The link function is \eqn{log((1 - b - \mu)/(\mu - a))}.
#' 
#' @note User-specified starting values may be necessary when the sensitivity and/or specificity are relatively low. One strategy is to obtain starting values from a model where the sensitivity and specificity are both specified as larger values (or one).
#' 
#' @examples
#' library(dplyr)
#' 
#' # simulate response with sensitivity of 0.8 and specificity of 0.9
#' d <- data.frame(x = seq(-3, 3, length = 1000)) %>% 
#'    mutate(y = rbinom(n(), 1, 0.8 * plogis(x) + (1 - 0.9) * plogis(-x)))
#'    
#' # estimate model with link function accounting for sensitivity and specificity    
#' m <- glm(y ~ x, family = binomial(link = logiterr(0.8, 0.9)), data = d)
#' summary(m)$coefficients
#' 
#' @importFrom stats plogis
#' @export
logiterr <- function(sens = 1, spec = 1) {
  linkfun <- function(mu) {
    log((1 - spec - mu)/(mu - sens))
  }
  linkinv <- function(eta) {
    sens * plogis(eta) + (1 - spec) * plogis(-eta) 
  }
  mu.eta <- function(eta) {
    (sens * exp(eta) + spec * exp(eta) - exp(eta)) / (exp(eta) + 1)^2
  }
  valideta <- function(eta) TRUE
  structure(list(linkfun = linkfun, linkinv = linkinv, mu.eta = mu.eta,
    valideta = valideta, name = "logiterr"), class = "link-glm")
}
