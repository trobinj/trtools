#' Confidence and prediction intervals for the (expected) response of a nonlinear regression model (experimental).
#' 
#' This function computes approximate confidence intervals for the expected response as well as prediction intervals for a nonlinear regression model. It is designed to behave similarly to the \code{predict} function. 
#' 
#' @param object Model object of class \code{nls}.
#' @param newdata An optional data frame in which to look for variables with which to predict. If omitted the data frame used in creating the model object is used. 
#' @param interval Type of interval calculation (confidence or prediction). 
#' @param fcov Function for estimating the variance-covariance matrix of the model parameters.
#' @param level Confidence level in (0,1).
#' @param residuals Logical to compute leverage values and standardized residuals. 
#' @param ... Arguments to pass to \code{fcov}.
#' 
#' @details Standard errors for confidence intervals are estimated using the delta method. Derivatives are computed numerically rather than analytically to permit a wider range of models. Prediction intervals assume normally-distributed responses with variance inversely proportional to any specified weights, or homoscedastic error if no weights are specified. The approximate leverage ("hat") values are computed using the jacobian matrix and the approximate standardized residuals are equivalent to the "internally" standardized residuals in a linear model (i.e., as would be returned by rstandard in a linear model or rstandard with type = "pearson" in a GLM).
#' 
#' @examples 
#' myreg <- nls(rate ~ (t1 + t3 * (state == "treated")) * conc /
#'  (t2 + t4 * (state == "treated") + conc), data = Puromycin,
#'  start = c(t1 = 150, t2 = 0, t3 = 0.05, t4 = 0)) 
#' nlsint(myreg, interval = "confidence")
#' @importFrom stats pt formula weights
#' @importFrom numDeriv jacobian
#' @export
nlsint <- function(object, newdata = eval(object$call$data), interval = c("confidence", "prediction"), fcov = vcov, level = 0.95, residuals = FALSE, ...) {
  if (class(object) != "nls") stop("nls objects only")
  type <- match.arg(interval)
  f <- function(theta, object, data) {
    theta <- as.list(theta)
    names(theta) <- names(coef(object))
    return(with(c(theta, data), eval(parse(text = as.character(formula(object))))))
  }
  dmat <- jacobian(f, coef(object), object = object, data = newdata)
  vmat <- fcov(object, ...)
  df <- summary(object)$df[2]
  yh <- f(coef(object), object, newdata)
  va <- diag(dmat %*% vmat %*% t(dmat))
  if (is.null(weights(object))) {
    wi <- 1
  }
  else {
    wi <- weights(object)
  }
  se <- switch(type,
    confidence = sqrt(va), 
    prediction = sqrt(va + summary(object)$sigma^2/wi)
  )
  lw <- yh - qt(level + (1 - level)/2, df) * se
  up <- yh + qt(level + (1 - level)/2, df) * se 
  out <- data.frame(fit = yh, se = se, lwr = lw, upr = up)
  if (residuals) {
    dw <- sweep(t(dmat), 2, wi, "*")
    out$hat <- diag(dmat %*% solve(dw %*% dmat) %*% dw)
    out$res <- residuals(object)/sqrt((1 - out$hat) * summary(object)$sigma^2/wi)
  }
  rownames(out) <- NULL
  return(out)
}