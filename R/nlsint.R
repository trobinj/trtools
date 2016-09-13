#' Confidence and prediction intervals for the expected/future response of a nonlinear regression model.
#'
#' \code{nlsint} computes approximate confidence intervals for the expected response and prediction intervals
#' for the future response of a nonlinear regression model of class \code{nls}. 
#' 
#' @param object An object of class \code{nls} (i.e., an estimated nonlinear regression model). 
#' @param newdata Data frame of variables at which to compute confidence/prediction intervals.
#' Defaults to the same data frame used to create \code{object}. 
#' @param interval Type of interval (confidence or prediction).
#' @param level Confidence/tolerance level. 
#' @return A data frame of the predicted response, the standard error or prediction error, and the lower
#' and upper bound of the confidence/prediction interval at each set of variable values in \code{newdata}. 
#' @details Standard errors and the estimated expected response are obtained using the delta method based
#' on the numerical derivatives and the estimated covariance matrix of the model parameters. The prediction 
#' use the estimated error variance and implicitly assumes identically distributed normal errors.
#' @examples 
#' model <- nls(rate ~ Vm * conc / (K + conc), start = c(Vm = 200, K = 0.1), data = subset(Puromycin, state == "treated"))
#' data <- data.frame(conc = seq(0.02, 1.10, by = 0.01))
#' nlsint(model, newdata = data)
#' nlsint(model, newdata = data, interval = "prediction") 
nlsint <- function(object, newdata = eval(object$call$data),
  interval = c("confidence", "prediction"), level = 0.95) {
  
  if (class(object) != "nls") stop("nls objects only")
  
  type <- match.arg(interval)
  
  p.names <- names(coef(object))       
  x.names <- names(object$dataClasses)
  
  for (i in 1:length(x.names)) {
      assign(x.names[i], newdata[[x.names[i]]])
  }

  f <- function(theta) {
    for (i in 1:length(p.names)) {
      assign(p.names[i], theta[i])
    }
    eval(parse(text = as.character(formula(object))[3]))
  }
  
  dmat <- jacobian(f, coef(object))
  vmat <- vcov(object)
  
  df <- summary(object)$df[2]
  yh <- f(coef(object))
  va <- diag(dmat %*% vmat %*% t(dmat))
  se <- switch(type,
    confidence = sqrt(va), 
    prediction = sqrt(va + summary(object)$sigma^2)
  )
  lw <- yh - qt(level + (1 - level)/2, df) * se
  up <- yh + qt(level + (1 - level)/2, df) * se 
  
  out <- data.frame(fit = yh, se = se, lwr = lw, upr = up)
  rownames(out) <- NULL
  return(out)
}


