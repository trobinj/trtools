#' Linear combinations of regression model parameters. 
#' 
#' This is a function allows one to obtain standard inferences (i.e., point estimates, standard errors, confidence intervals, etc.) concerning any linear combination of regression model parameters.
#' 
#' @param model Model object. Currently only objects of class \code{lm} and \code{nls} are accepted.
#' @param a Vector or matrix defining the linear combination(s). If omitted then this defaults to the identity matrix to provide inferences for each parameter similar to the \code{summary} function.  
#' @param cnames Optional vector of contrast names. If left missing the contrast coefficients are shown. If FALSE then no names are shown. 
#' @param level Confidence level in (0,1).
#' @param fcov Function for estimating the variance-covariance matrix of the model parameters.
#' @param ... Arguments to pass to \code{fcov}.
#' 
#' @examples
#' myreg <- lm(Gas ~ Insul + Temp + Insul:Temp, data = MASS::whiteside)
#' # same as summary(myreg)
#' lincon(myreg)
#' # slope with respect to temperature after insulation
#' lincon(myreg, a = c(0,0,1,1))
#' # change in expected gas consumption at five degrees celsius from adding insulation
#' lincon(myreg, a = c(0,1,0,5))
#'
#' @details For a regression model with parameters \eqn{\beta_0, \beta_1, \dots, \beta_p} a linear combination is defined as \eqn{a_0\beta_0 + a_1\beta_1 + \cdots + a_p\beta_p}. Inferences are based on either exact (normal theory) or Wald (asymptotic) test statistics and confidence intervals. The estimated standard error(s) of the linear combinations are computed using any specified function for estimating the variance-covariance matrix of the model parameters. 
#' @importFrom stats pt
#' @importFrom MASS fractions
#' @export
lincon <- function(model, a, cnames, level = 0.95, fcov = vcov, ...) {
  if (!(class(model) %in% c("lm","nls"))) {
    stop("function currently only works for lm and nls objects")
  }
  if (missing(a)) {
    a <- diag(length(coef(model)))
    if (missing(cnames)) {
      cnames <- names(coef(model))  
    }
  }
  else if (is.vector(a)) {
    a <- matrix(a, nrow = 1)
  }
  se <- sqrt(diag(a %*% fcov(model, ...) %*% t(a)))
  pe <- a %*% coef(model)
  df <- summary(model)$df[2]
  lw <- pe - qt(level + (1 - level)/2, df) * se
  up <- pe + qt(level + (1 - level)/2, df) * se 
  ts <- pe/se
  pv <- 2*pt(-abs(ts), df)
  out <- cbind(pe, se, lw, up, ts, df, pv)
  colnames(out) <- c("Estimate", "Std. Error", "Lower", "Upper", "t value", "df", "Pr(>|t|)")
  if (missing(cnames)) {
    rownames(out) <- apply(a, 1, function(x) paste("(", paste(fractions(as.vector(x)), collapse = ","), ")", sep = ""))
  }
  else if (is.logical(cnames) && !cnames) {
    rownames(out) <- rep("", nrow(a))
  }
  else {
    rownames(out) <- cnames
  }
  return(out)
}