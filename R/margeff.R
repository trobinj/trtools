#' Estimation of marginal effects of regression models (experimental). 
#' 
#' This function estimates discrete marginal effects (i.e., changes in the expected response) for (generalized) linear and nonlinear regression models as the difference or ratio of expected responses. It can also approximate "instantaneous" marginal effects. The standard error of the marginal effect is computed using the delta method with numerical differentiation.
#' 
#' @param model Model object of class \code{lm}, \code{glm}, or \code{nls}.
#' @param a List or data frame defining values of the explanatory variables.
#' @param b List or data frame defining values of the explanatory variables.
#' @param df Degrees of freedom for the confidence interval. If omitted it is extracted from the model object.
#' @param cnames Optional names for the marginal effects.
#' @param type Type of marginal effect (difference, percent, or factor). Difference is the default. 
#' @param delta Divisor for the marginal effect (default is one). This has no effect unless \code{type = "difference"}. 
#' @param level Confidence level in (0,1).
#' @param fcov Function for estimating the variance-covariance matrix of the model parameters.
#' 
#' @details A (discrete) marginal effect is defined as the difference \eqn{[E(Y|X = a) - E(Y|X = b)]/\delta} where \eqn{a} and \eqn{b} represent specified values of the explanatory variables. Typically these differ only in terms of the value of one explanatory variable to estimate the marginal effect of changing that explanatory variable when the other explanatory variables are held constant at specified values. Also typically \eqn{\delta} would be set to one unless a change of scale is desired or one wishes to approximate the instantaneous marginal effect (see below). 
#' 
#' For continuous explanatory variables the "instantaneous" marginal effect is a limiting case of the discrete marginal effect. For example, if there are two explanatory variables --- \eqn{X_1} and \eqn{X_2} --- the marginal effect of \eqn{X_1} at \eqn{X_1 = x_1} and \eqn{X_2 = x_2} can be defined as the limit of \eqn{[E(Y|X_1 = x_1 + \delta, X_2 = x_2) - E(Y|X_1 = x_1, X_2 = x_2)]/\delta} as \eqn{\delta} goes to zero (i.e., the derivative \eqn{E(Y|X_1=x_1,X_2=x_2)} with respect to \eqn{x_1}). This can be approximated numerically by setting \eqn{\delta} to a relatively small number.
#' 
#' A marginal effect can also be defined as the percent change in the expected response,  \eqn{100[E(Y|X = a) - E(Y|X = b)]/E(Y|X = b)}. A positive value is the percent increase in the expected response, and a negative value is the percent decrease in the expected response. 
#' 
#' Finally the marginal effect can be defined by the factor \eqn{[E(Y|X = a)/E(Y|X = b)]}. 
#' 
#' @examples 
#' m <- glm(cbind(deaths, total - deaths) ~ insecticide * log2(deposit), 
#'  family = binomial, data = insecticide) 
#' # discrete marginal effect of increasing deposit from 4 to 6 for each insecticide  
#' margeff(m, 
#'  a = list(deposit = 6, insecticide = levels(insecticide$insecticide)),
#'  b = list(deposit = 4, insecticide = levels(insecticide$insecticide)),
#'  cnames = levels(insecticide$insecticide))
#' # approximate "instantaneous" marginal effects for each insecticide at a deposit of 5 
#' margeff(m, 
#'  a = list(deposit = 5 + 0.001, insecticide = levels(insecticide$insecticide)),
#'  b = list(deposit = 5, insecticide = levels(insecticide$insecticide)),
#'  cnames = levels(insecticide$insecticide), delta = 0.001)
#' # percent change in expected proportion of deaths when increasing deposit from 4 to 6
#' margeff(m, 
#'  a = list(deposit = 6, insecticide = levels(insecticide$insecticide)),
#'  b = list(deposit = 4, insecticide = levels(insecticide$insecticide)),
#'  cnames = levels(insecticide$insecticide), type = "percent")
#' @importFrom stats predict
#' @export
margeff <- function(model, a, b, df, cnames, type = c("difference", "percent", "factor"), delta = 1, level = 0.95, fcov = vcov, ...) {
  if (!any(class(model) %in% c("lm","glm","nls","lmerMod","glmerMod"))) {
    stop("function currently only works for lm, glm, nls, lmerMod, and glmerMod objects")
  }
  type <- match.arg(type)
  if ("nls" %in% class(model)) {
    f <- function(theta, model, a, b, type, delta) {
      theta <- as.list(theta)
      names(theta) <- names(coef(model))
      pa <- with(c(theta, as.data.frame(a)), eval(parse(text = as.character(formula(model)))))
      pb <- with(c(theta, as.data.frame(b)), eval(parse(text = as.character(formula(model)))))
      out <- switch(type,
       difference = (pa - pb)/delta,
       percent = 100 * (pa/pb - 1),
       factor = pa/pb)
      return(out)
    }
  }
  else if (length(intersect(class(model), c("lmerMod","glmerMod")) > 0)) {
    f <- function(theta, model, a, b, type, delta) {
      if (summary(model)$useScale) {
        pa <- predict(model, as.data.frame(a), re.form = NA,
          newparams = list(beta = theta, sigma = sigma(model), theta = getME(model, "theta")))
        pb <- predict(model, as.data.frame(b), re.form = NA,
          newparams = list(beta = theta, sigma = sigma(model), theta = getME(model, "theta")))
      }
      out <- switch(type,
        difference = (pa - pb)/delta,
        percent = 100 * (pa/pb - 1),
        factor = pa/pb)
      return(out)
    }
  }
  else {
    f <- function(theta, model, a, b, type, delta) {
      model$coefficients <- theta
      pa <- predict(model, as.data.frame(a), type = "response")
      pb <- predict(model, as.data.frame(b), type = "response")
      out <- switch(type,
        difference = (pa - pb)/delta,
        percent = 100 * (pa/pb - 1),
        factor = pa/pb)
      return(out)
    }    
  }
  if (missing(df)) {
    if (("glm" %in% class(model)) && (family(model)[1] %in% c("binomial","poisson"))) {
      df <- Inf
    }
    else if (length(intersect(class(model), c("lmerMod","glmerMod")) > 0)) {
      df <- Inf
    }
    else {
      df <- summary(model)$df[2]
    }
  }
  if (length(intersect(class(model), c("lmerMod","glmerMod")) > 0)) {
    if (summary(m)$useScale) {
      paramlist <- list(beta = fixef(model), sigma = sigma(model), theta = getME(model, "theta"))
    } else {
      paramlist <- list(beta = fixef(model), theta = getME(model, "theta"))
    }
    pa <- predict(model, as.data.frame(a), re.form = NA, newparams = paramlist)
    pb <- predict(model, as.data.frame(b), re.form = NA, newparams = paramlist)
    gr <- numDeriv::jacobian(f, fixef(model), model = model, a = a, b = b, 
      type = type, delta = delta)
  } else {
    pa <- predict(model, as.data.frame(a), type = "response")
    pb <- predict(model, as.data.frame(b), type = "response")
    gr <- numDeriv::jacobian(f, coef(model), model = model, a = a, b = b, 
      type = type, delta = delta)
  }
  pe <- switch(type,
    difference = (pa - pb)/delta,
    percent = 100 * (pa/pb - 1),
    factor = pa/pb)
  se <- sqrt(diag(gr %*% fcov(model) %*% t(gr)))
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





