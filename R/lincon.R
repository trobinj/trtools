#' Linear combinations of regression model parameters. 
#' 
#' This is a function allows one to obtain standard inferences (i.e., point estimates, standard errors, confidence intervals, etc.) concerning any linear combination of regression model parameters.
#' 
#' @aliases lincon.lm lincon.nls lincon.glm lincon.lmerMod lincon.glmerMod
#' 
#' @usage
#' \method{lincon}{lm}(model, a, b, df, tf, cnames, level = 0.95, fcov = vcov, ...)
#' \method{lincon}{nls}(model, a, b, df, tf, cnames, level = 0.95, fcov = vcov, ...)
#' \method{lincon}{glm}(model, a, b, df, tf, cnames, level = 0.95, fcov = vcov, ...)
#' \method{lincon}{lmerMod}(model, a, b, df, tf, cnames, level = 0.95, fcov = vcov, ...)
#' \method{lincon}{glmerMod}(model, a, b, df, tf, cnames, level = 0.95, fcov = vcov, ...)
#' 
#' @param model Model object. Currently only objects of class \code{lm} and \code{nls} are accepted.
#' @param a Vector or matrix defining the \eqn{a_j} coefficients of the linear combination(s). If omitted then this defaults to the identity matrix to provide inferences for each parameter similar to the \code{summary} function. 
#' @param b A scalar or vector defining the \eqn{b} coefficient(s) of the linear combination. Assumed to be zero if missing. 
#' @param df Optional degrees of freedom. If left missing the residual degrees of freedom will be used except for GLMs with \code{family = poisson} or \code{family = binomial} in which case an infinite degrees of freedom is used.  
#' @param tf Optional transformation function to apply to the point estimate(s) and confidence interval limits (e.g., \code{tf = exp} for a logistic model to estimate odds or odds ratios). 
#' @param cnames Optional vector of contrast names. If left missing the contrast coefficients are shown. If FALSE then no names are shown. 
#' @param level Confidence level in (0,1). Default is 0.95.
#' @param fcov Function for estimating the variance-covariance matrix of the model parameters.
#' @param ... Not used.
#' 
#' @examples
#' myreg <- lm(Gas ~ Insul + Temp + Insul:Temp, data = MASS::whiteside)
#' # same as summary(myreg)
#' lincon(myreg)
#' # slope with respect to temperature after insulation
#' lincon(myreg, a = c(0,0,1,1))
#' # expected gas consumption before and after insulation at 5 degrees celsius
#' lincon(myreg, a = matrix(c(1,0,5,0,1,1,5,5), 2, 4, byrow = TRUE), cnames = c("Before","After"))
#' # change in expected gas consumption at five degrees celsius from adding insulation
#' lincon(myreg, a = c(0,1,0,5))
#'
#' @details For a regression model with a linear component (e.g., linear and generalized linear models) with parameters \eqn{\beta_0, \beta_1, \dots, \beta_p} a linear combination is defined as \deqn{a_0\beta_0 + a_1\beta_1 + \cdots + a_p\beta_p + b.} For a nonlinear regression model with parameters \eqn{\theta_1, \theta_2, \dots, \theta_q} a linear combination is defined as \deqn{a_1\theta_1 + a_2\theta_2 + \cdots + a_q\theta_q + b.} Inferences for the linear combination are based on either exact (normal theory) or Wald (asymptotic) test statistics and confidence intervals. The estimated standard error(s) of the linear combinations are computed using any specified function for estimating the variance-covariance matrix of the model parameters. 
#' @importFrom stats vcov coef family qt pt
#' @importFrom MASS fractions
#' @import lme4
#' @export
lincon <- function(model, ...) {
  UseMethod("lincon", model)
}
#' @export 
lincon.lm <- function(model, a, b, df, tf, cnames, level = 0.95, fcov = vcov, ...) {
  if (missing(a)) {
    a <- diag(length(coef(model)))
    if (missing(cnames)) {
      cnames <- names(coef(model))  
    }
  }
  else if (is.vector(a)) {
    a <- matrix(a, nrow = 1)
  }
  if (missing(b)) {
    b <- 0 
  }
  se <- sqrt(diag(a %*% fcov(model) %*% t(a)))
  pe <- a %*% coef(model) + b
  if (missing(df)) {
      df <- summary(model)$df[2]
  }
  lw <- pe - qt(level + (1 - level)/2, df) * se
  up <- pe + qt(level + (1 - level)/2, df) * se 
  ts <- pe/se
  pv <- 2*pt(-abs(ts), df)
  if (!missing(tf)) {
    if (any(tf(lw) > tf(up))) {
      tmp <- lw
      lw <- up
      up <- tmp
    }
    out <- cbind(tf(pe), tf(lw), tf(up))
    colnames(out) <- c("estimate", "lower", "upper")
  }
  else {
    out <- cbind(pe, se, lw, up, ts, df, pv)
    colnames(out) <- c("estimate", "se", "lower", "upper", "tvalue", "df", "pvalue")
  }
  if (missing(cnames)) {
    rownames(out) <- paste(apply(a, 1, function(x) paste("(", paste(MASS::fractions(as.vector(x)), collapse = ","), ")", sep = "")), ",", b, sep = "")
  }
  else if (is.logical(cnames) && !cnames) {
    rownames(out) <- rep("", nrow(a))
  }
  else {
    rownames(out) <- cnames
  }
  return(out)
}
#' @export
lincon.nls <- function(model, a, b, df, tf, cnames, level = 0.95, fcov = vcov, ...) {
  if (missing(a)) {
    a <- diag(length(coef(model)))
    if (missing(cnames)) {
      cnames <- names(coef(model))  
    }
  }
  else if (is.vector(a)) {
    a <- matrix(a, nrow = 1)
  }
  if (missing(b)) {
    b <- 0 
  }
  se <- sqrt(diag(a %*% fcov(model) %*% t(a)))
  pe <- a %*% coef(model) + b
  if (missing(df)) {
    df <- summary(model)$df[2]
  }
  lw <- pe - qt(level + (1 - level)/2, df) * se
  up <- pe + qt(level + (1 - level)/2, df) * se 
  ts <- pe/se
  pv <- 2*pt(-abs(ts), df)
  if (!missing(tf)) {
    if (any(tf(lw) > tf(up))) {
      tmp <- lw
      lw <- up
      up <- tmp
    }
    out <- cbind(tf(pe), tf(lw), tf(up))
    colnames(out) <- c("estimate", "lower", "upper")
  }
  else {
    out <- cbind(pe, se, lw, up, ts, df, pv)
    colnames(out) <- c("estimate", "se", "lower", "upper", "tvalue", "df", "pvalue")
  }
  if (missing(cnames)) {
    rownames(out) <- paste(apply(a, 1, function(x) paste("(", paste(MASS::fractions(as.vector(x)), collapse = ","), ")", sep = "")), ",", b, sep = "")
  }
  else if (is.logical(cnames) && !cnames) {
    rownames(out) <- rep("", nrow(a))
  }
  else {
    rownames(out) <- cnames
  }
  return(out)
}
#' @export 
lincon.glm <- function(model, a, b, df, tf, cnames, level = 0.95, fcov = vcov, ...) {
  if (missing(a)) {
    a <- diag(length(coef(model)))
    if (missing(cnames)) {
      cnames <- names(coef(model))  
    }
  }
  else if (is.vector(a)) {
    a <- matrix(a, nrow = 1)
  }
  if (missing(b)) {
    b <- 0 
  }
  se <- sqrt(diag(a %*% as.matrix(fcov(model)) %*% t(a)))
  pe <- a %*% coef(model) + b
  if (missing(df)) {
    if (family(model)[1] %in% c("binomial","poisson")) {
      df <- Inf
    }
    else {
      df <- summary(model)$df[2]
    }
  }
  lw <- pe - qt(level + (1 - level)/2, df) * se
  up <- pe + qt(level + (1 - level)/2, df) * se 
  ts <- pe/se
  pv <- 2*pt(-abs(ts), df)
  if (!missing(tf)) {
    if (any(tf(lw) > tf(up))) {
      tmp <- lw
      lw <- up
      up <- tmp
    }
    out <- cbind(tf(pe), tf(lw), tf(up))
    colnames(out) <- c("estimate", "lower", "upper")
  }
  else {
    out <- cbind(pe, se, lw, up, ts, df, pv)
    colnames(out) <- c("estimate", "se", "lower", "upper", "tvalue", "df", "pvalue")
  }
  if (missing(cnames)) {
    rownames(out) <- paste(apply(a, 1, function(x) paste("(", paste(MASS::fractions(as.vector(x)), collapse = ","), ")", sep = "")), ",", b, sep = "")
  }
  else if (is.logical(cnames) && !cnames) {
    rownames(out) <- rep("", nrow(a))
  }
  else {
    rownames(out) <- cnames
  }
  return(out)
}
#' @export
lincon.lmerMod <- function(model, a, b, df, tf, cnames, level = 0.95, fcov = vcov, ...) {
  if (missing(a)) {
    a <- diag(length(fixef(model)))
    if (missing(cnames)) {
      cnames <- names(fixef(model))  
    }
  }
  else if (is.vector(a)) {
    a <- matrix(a, nrow = 1)
  }
  if (missing(b)) {
    b <- 0 
  }
  se <- sqrt(diag(a %*% as.matrix(fcov(model)) %*% t(a)))
  pe <- a %*% fixef(model) + b
  if (missing(df)) {
    df <- Inf
  }
  lw <- pe - qt(level + (1 - level)/2, df) * se
  up <- pe + qt(level + (1 - level)/2, df) * se 
  ts <- pe/se
  pv <- 2*pt(-abs(ts), df)
  if (!missing(tf)) {
    if (any(tf(lw) > tf(up))) {
      tmp <- lw
      lw <- up
      up <- tmp
    }
    out <- cbind(tf(pe), tf(lw), tf(up))
    colnames(out) <- c("estimate", "lower", "upper")
  }
  else {
    out <- cbind(pe, se, lw, up, ts, df, pv)
    colnames(out) <- c("estimate", "se", "lower", "upper", "tvalue", "df", "pvalue")
  }
  if (missing(cnames)) {
    rownames(out) <- paste(apply(a, 1, function(x) paste("(", paste(MASS::fractions(as.vector(x)), collapse = ","), ")", sep = "")), ",", b, sep = "")
  }
  else if (is.logical(cnames) && !cnames) {
    rownames(out) <- rep("", nrow(a))
  }
  else {
    rownames(out) <- cnames
  }
  return(out)
}
#' @export 
lincon.glmerMod <- function(model, a, b, df, tf, cnames, level = 0.95, fcov = vcov, ...) {
  if (missing(a)) {
    a <- diag(length(fixef(model)))
    if (missing(cnames)) {
      cnames <- names(fixef(model))  
    }
  }
  else if (is.vector(a)) {
    a <- matrix(a, nrow = 1)
  }
  if (missing(b)) {
    b <- 0 
  }
  se <- sqrt(diag(a %*% as.matrix(fcov(model)) %*% t(a)))
  pe <- a %*% fixef(model) + b
  if (missing(df)) {
    df <- Inf
  }
  lw <- pe - qt(level + (1 - level)/2, df) * se
  up <- pe + qt(level + (1 - level)/2, df) * se 
  ts <- pe/se
  pv <- 2*pt(-abs(ts), df)
  if (!missing(tf)) {
    if (any(tf(lw) > tf(up))) {
      tmp <- lw
      lw <- up
      up <- tmp
    }
    out <- cbind(tf(pe), tf(lw), tf(up))
    colnames(out) <- c("estimate", "lower", "upper")
  }
  else {
    out <- cbind(pe, se, lw, up, ts, df, pv)
    colnames(out) <- c("estimate", "se", "lower", "upper", "tvalue", "df", "pvalue")
  }
  if (missing(cnames)) {
    rownames(out) <- paste(apply(a, 1, function(x) paste("(", paste(MASS::fractions(as.vector(x)), collapse = ","), ")", sep = "")), ",", b, sep = "")
  }
  else if (is.logical(cnames) && !cnames) {
    rownames(out) <- rep("", nrow(a))
  }
  else {
    rownames(out) <- cnames
  }
  return(out)
}