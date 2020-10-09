#' Link function for composite sampling designs with an underlying logistic model.
#' 
#' This link function is applicable to composite sampling designs. The assumption is that there exists an underlying binomially-distributed response \eqn{Z} that follows a logistic regression model, but we only observe \eqn{Y = I(Z > 0)}. Thus \eqn{log[E(Y/m)/(1-E(Y/m))] = \eta}, which implies that \eqn{E(Y) = 1 - (1 - \pi)^m} where \eqn{\pi = E(Y/m) = 1/(1 + exp(\eta))} and so the link function for \eqn{Y} is \eqn{\eta = log((1-\pi)^(-1/m) - 1)}.
#' 
#' @details The argument \code{m} specifies the number of trials for \eqn{Y}. This can be a scalar or a variable in the data frame, but if it is the latter then it is necessary explicitly specify the data frame (see example below). 
#' @examples
#' library(dplyr)
#'
#' d <- data.frame(m = sample(c(25,50,100,400), 100, replace = TRUE)) %>% 
#'   mutate(x = seq(-2, 2, length = n())) %>% 
#'   mutate(y = rbinom(n(), m, plogis(qlogis(0.05) + x/5)))
#'
#' # Model for underlying binomially-distributed response Z.
#' m <- glm(cbind(y, m - y) ~ x, family = binomial, data = d)
#' summary(m)$coefficients
#'
#' d <- d %>% mutate(y = as.numeric(y > 0))
#'
#' # Model for Y = I(Z > 0). 
#' m <- glm(y ~ x, family = binomial(link = complogit(d$m)), data = d)
#' summary(m)$coefficients
#' @export
complogit <- function(m) {
  linkfun <- function(y) {
    log((1 - y)^(-1/m) - 1)
  }
  linkinv <- function(eta) {
    1 - (1 - 1/(1 + exp(-eta)))^m
  }
  mu.eta <- function(eta) {
    m * exp(eta) * (1 / (exp(eta) + 1))^(m + 1)
  }
  valideta <- function(eta) TRUE
  link <- "log((1 - y)^(-1/m) - 1)"
  structure(list(linkfun = linkfun, linkinv = linkinv, 
    mu.eta = mu.eta, valideta = valideta, name = link), class = "link-glm")
}

