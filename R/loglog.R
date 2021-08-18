#' Log-log link function for generalized linear models.
#' 
#' This is the log-log link function for generalized linear models. 
#' 
#' @details The log-log link function is defined as \eqn{-log[-log[E(Y)]]}. Note that using the log-log link function for the probability of "successes" is the same as using the complementary log-log link function for the probability of "failures" in a generalized linear model. The coefficients from the two models only differ by sign.  
#' 
#' @note Some authors define the log-log link function as \eqn{log[-log[E(Y)]} and the inverse link function is \eqn{exp[-exp(\eta)]} so that in a GLM the signs of the parameters are reversed and are equal to the parameters of a model with a complementary log-log link function for the probabilities of failures.
#' 
#' @examples
#' # the following will produce the same results up to a reversal of the sign of the coefficients
#' model.loglog <- glm(cbind(y, total-y) ~ density + species, 
#'  family = binomial(link = loglog), data = rotifer)
#' model.cloglog <- glm(cbind(total-y, y) ~ density + species, 
#'  family = binomial(link = cloglog), data = rotifer)
#' @export
loglog <- structure(list(
  linkfun = function(mu) -log(-log(mu)),
  linkinv = function(eta) exp(-exp(-eta)),
  mu.eta = function(eta) exp(-exp(-eta) - eta),
  valideta = function(eta) TRUE,
  name = "loglog"), class = "link-glm")