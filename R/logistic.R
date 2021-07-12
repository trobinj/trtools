logistic <- function(a = 0, b = 1, c = 1) {
  linkfun <- function(mu) {
    u <- ((mu - a)/(b - a))^(1/c)
    log(u / (1 - u))
  }
  linkinv <- function(eta) {
    a + (b - a) * plogis(eta)^c
  }
  mu.eta <- function(eta) {
    c * (b - a) * plogis(eta)^(c - 1) * dlogis(eta)
  }
  valideta <- function(eta) TRUE
  structure(list(linkfun = linkfun, linkinv = linkinv, mu.eta = mu.eta,
    valideta = valideta, name = "logistic"), class = "link-glm")
}