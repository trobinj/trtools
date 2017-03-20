#' Estimation of marginal effects of regression models (experimental). 
#' 
#' This function estimates discrete marginal effects (i.e., changes in the expected response) for (generalized) linear and nonlinear regression models. It can also approximate "instantaneous" marginal effects. The standard error of the marginal effect is computed using the delta method with numerical differentiation.
#' 
#' @param model Model object of class \code{lm}, \code{glm}, or \code{nls}.
#' @param a List or data frame defining values of the explanatory variables.
#' @param b List or data frame defining values of the explanatory variables.
#' @param df Degrees of freedom for the confidence interval. If omitted it is extracted from the model object.
#' @param cnames Optional names for the marginal effects.
#' @param percent Logical for whether or not to compute percent change (default is FALSE). 
#' @param delta Divisor for the marginal effect (default is one). This has no effect of \code{percent = TRUE}. 
#' @param level Confidence level in (0,1).
#' @param fcov Function for estimating the variance-covariance matrix of the model parameters.
#' @param ... Arguments to pass to \code{fcov}.
#' 
#' @details A (discrete) marginal effect is defined as \eqn{[E(Y|X = a) - E(Y|X = b)]/\delta} where \eqn{a} and \eqn{b} represent specified values of the explanatory variables. Typically these differ only in terms of the value of one explanatory variable to estimate the marginal effect of changing that explanatory variable when the other explanatory variables are held constant at specified values. 
#' 
#' For continuous explanatory variables the "instantaneous" marginal effect is a limiting case of the discrete marginal effect. For example, if there are two explanatory variables --- \eqn{X_1} and \eqn{X_2} --- the marginal effect of \eqn{X_1} at \eqn{X_1 = x_1} and \eqn{X_2 = x_2} can be defined as the limit of \eqn{[E(Y|X_1 = x_1 + \delta, X_2 = x_2) - E(Y|X_1 = x_1, X_2 = x_2)]/\delta} as \eqn{\delta} goes to zero (i.e., the derivative \eqn{E(Y|X_1=x_1,X_2=x_2)} with respect to \eqn{x_1}). This can be approximated accurately by setting \eqn{\delta} to a sufficiently small number. 
#' 
#' The discrete marginal effect can also be defined as the percent change in the expected response,  \eqn{100[E(Y|X = a) - E(Y|X = b)]/E(Y|X = b)}. A positive value is the percent increase in the expected response, and a negative value is the percent decrease in the expected response. 
#' 
#' @examples 
#' m <- glm(cbind(deaths, total - deaths) ~ insecticide * deposit, 
#'  family = binomial, data = insecticide) 
#' # discrete marginal effect of increasing deposit from 4 to 6 for each insecticide  
#' margeff(m, 
#'  a = list(deposit = 6, insectide = levels(insecticide$insecticide)),
#'  b = list(deposit = 4, insectide = levels(insecticide$insecticide)),
#'  cnames = levels(insecticide$insecticide))
#' # approximate "instantaneous" marginal effects for each insecticide at a deposit of 5 
#' margeff(m, 
#'  a = list(deposit = 5 + 0.001, insectide = levels(insecticide$insecticide)),
#'  b = list(deposit = 5, insectide = levels(insecticide$insecticide)),
#'  cnames = levels(insecticide$insecticide), delta = 0.001)
#' # percent change in expected proportion of deaths when increasing deposit from 4 to 6
#' margeff(m, 
#'  a = list(deposit = 6, insectide = levels(insecticide$insecticide)),
#'  b = list(deposit = 4, insectide = levels(insecticide$insecticide)),
#'  cnames = levels(insecticide$insecticide), percent = TRUE)
#' @importFrom stats predict
#' @export
margeff <- function(model, a, b, df, cnames, percent = FALSE, 
  delta = 1, level = 0.95, fcov = vcov, ...) {
  if (!any(class(model) %in% c("lm","glm"))) {
    stop("function currently only works for lm and glm objects")
  }
  f <- function(theta, model, a, b, delta, percent) {
    model$coefficients <- theta
    pa <- predict(model, as.data.frame(a), type = "response")
    pb <- predict(model, as.data.frame(b), type = "response")
    return((pa - pb)/delta / (percent * pb / 100 + (1 - percent) * delta))
  }
  if (missing(df)) {
    if (("glm" %in% class(model)) && (family(model)[1] %in% c("binomial","poisson"))) {
      df <- Inf
    }
    else {
      df <- summary(model)$df[2]
    }
  }
  pa <- predict(model, as.data.frame(a), type = "response")
  pb <- predict(model, as.data.frame(b), type = "response")
  pe <- (pa - pb)/delta / (percent * pb / 100 + (1 - percent) * delta)
  gr <- numDeriv::jacobian(f, coef(model), model = model, a = a, b = b, 
    delta = delta, percent = percent)
  se <- sqrt(diag(gr %*% fcov(model, ...) %*% t(gr)))
  lw <- pe - qt(level + (1 - level)/2, df) * se
  up <- pe + qt(level + (1 - level)/2, df) * se 
  ts <- pe/se
  pv <- 2*pt(-abs(ts), df)
  out <- cbind(pe, se, lw, up, ts, df, pv)
  colnames(out) <- c("estimate", "se", "lower", "upper", "tvalue", "df", "pvalue")
  if (missing(cnames)) {
    rownames(out) <- rep("", nrow(out))
  }
  else {
    rownames(out) <- cnames
  }
  return(out)
}





