#' Log-log link function for generalized linear models.
#' 
#' This is the log-log link function for generalized linear models, usually with a binomially-distributed response variable. 
#' 
#' @details The log-log link function is defined as \eqn{\log[-\log[E(Y_i)]]}, where \eqn{Y_i} is the response variable (i.e., a proportion in the case of a binomial distribution). Note that using the log-log link function for the proportion of ``successes'' is the same as using the complementary log-log link function for the proportion of ``failures'' in a generalized linear model. The coefficients from the two models only differ by sign.  
#' 
#' @examples
#' # the following produce the same results up to a reversal of the sign of the coefficients
#' model.loglog <- glm(cbind(y, total-y) ~ density + species, 
#'  family = binomial(link = loglog), data = rotifer)
#' model.cloglog <- glm(cbind(total-y, y) ~ density + species, 
#'  family = binomial(link = cloglog), data = rotifer)
#' @export
loglog <- structure(list(
  linkfun = function(mu) -log(-log(mu)),
  linkinv = function(eta)
    pmax(pmin(exp(-exp(-eta)), 1 - .Machine$double.eps), .Machine$double.eps),
  mu.eta = function(eta) {
    eta <- pmin(eta, 700)
    pmax(exp(-eta - exp(-eta)), .Machine$double.eps)
  },
  dmu.deta = function(eta)
    pmax(exp(-exp(-eta) - eta) * expm1(-eta), .Machine$double.eps),
  valideta = function(eta) TRUE,
  name = "loglog"
), class = "link-glm") 