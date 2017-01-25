#' Linear combinations of regression model parameters. 
#' 
#' This is a function allows one to obtain standard inferences (i.e., point estimates, standard errors, confidence intervals, etc.) concerning any linear combination of regression model parameters.
#' 
#' @param model Model object. Currently only objects of class \code{lm} are accepted.
#' @param a Vector or matrix defining the linear combination(s).
#' @param level Confidence level in (0,1).
#' @param fcov Function for estimating the variance-covariance matrix of the model parameters.
#' @param ... Arguments to pass to \code{fcov}.
#' 
#' @details For a regression model with parameters \eqn{\beta_0, \beta_1, \dots, \beta_p} a linear combination is defined as \eqn{a_0\beta_0 + a_1\beta_1 + \cdots + a_p\beta_p}. Inferences are based on either exact or Wald test statistics and confidence intervals. The estimated standard error(s) of the linear combinations are computed using any specified function for estimating the variance-covariance matrix of the model parameters. 
#' 
#' @export
lincon <- function(model, a, level = 0.95, fcov = vcov, ...) {
  if (class(model) != "lm") stop("function currently only works for lm objects")
  if (is.vector(a)) a <- matrix(a, nrow = 1)
  se <- sqrt(diag(a %*% fcov(model, ...) %*% t(a)))
  pe <- a %*% coef(model)
  df <- summary(model)$df[2]
  lw <- pe - qt(level + (1 - level)/2, df) * se
  up <- pe + qt(level + (1 - level)/2, df) * se 
  ts <- pe/se
  pv <- 2*pt(-abs(ts), df)
  out <- cbind(pe, se, ts, pv, lw, up)
  colnames(out) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)", "lower", "upper")
  rownames(out) <- apply(a, 1, paste, collapse = ",")
  return(out)
}