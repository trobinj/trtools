#' Delta method for regression model objects (experimental).
#' 
#' This function applies the "delta method" to functions of parameters of model objects, provided that functions can be specified to extract the parameter estimates and the (estimated) covariance matrix of the estimator from the object. Derivatives are computed using numerical (not symbolic) differentiation. Alternatively standard errors can be approximated using a bootstrap approach described by Mandel (2013). 
#' 
#' @param object Model object. Just about any object can be specified provided that functions can also be specified to extract the parameter estimates and the estimated covariance matrix of the estimators.
#' @param pfunc Character object that is function of the parameters that can be evaluated by R. The function can return a scalar or a vector. Parameters must be referenced by name as specified by \code{pname}. 
#' @param pname Names of the parameters extracted by \code{cfunc}.
#' @param cfunc Function for extracting the parameter estimates from \code{object} (default is \code{coef}).
#' @param vfunc Function for extracting the estimated covariance matrix of the estimator from \code{object} (default is \code{vcov}).
#' @param tfunc Function for transforming the result of \code{pfunc} for cases when the sampling distribution is thought to be more approximately normal on some other scale (e.g., log).
#' @param B Number of bootstrap samples. The default (\code{B = 0}) results in numerical differentiation instead. 
#' @param level Confidence level in (0,1) (default is 0.95). 
#' @param ... Optional arguments for \code{jacobian}. 
#' 
#' @details By default the function applies the delta method using numerical differentiation. However if \code{B} > 0 then a bootstrap method described by Mandel (2013) which avoids needing to compute derivatives.
#' 
#' @source Mandel, M. (2013). Simulation-based confidence intervals for functions with complicated derivatives. \emph{The American Statistician}, \emph{62}, 76-81.
#' 
#' @importFrom numDeriv jacobian
#' @importFrom MASS mvrnorm
#' @importFrom stats qnorm
#' @importFrom lme4 fixef
#' @export
dmethod <- function(object, pfunc, pname, cfunc = coef, vfunc = vcov, tfunc, B = 0, level = 0.95, ...) {
  f <- function(theta, pfunc, pname) {
    theta <- as.list(theta)
    names(theta) <- pname
    return(with(theta, eval(parse(text = pfunc))))
  }
  pe <- f(cfunc(object), pfunc, pname)
  if (B == 0) {
    ja <- numDeriv::jacobian(f, cfunc(object), pfunc = pfunc, pname = pname, ...)
    va <- diag(ja %*% vfunc(object) %*% t(ja))
  }
  else {
    y <- data.frame(MASS::mvrnorm(B, cfunc(object), vfunc(object)))
    colnames(y) <- pname
    y <- lapply(split(y, seq(B)), function(z) with(z, eval(parse(text = pfunc))))
    va <- apply(sweep(do.call("rbind", y), 2, pe), 2, function(z) sum(z^2)/B)
  }  
  se <- sqrt(va)
  lw <- pe - qnorm(level + (1 - level)/2) * se
  up <- pe + qnorm(level + (1 - level)/2) * se
  if (!missing(tfunc)) {
    if (any(tfunc(lw) > tfunc(up))) {
      tmp <- lw
      lw <- up
      up <- tmp
    }
    out <- cbind(tfunc(pe), se, tfunc(lw), tfunc(up))
    message("Note: Point estimates and confidence interval endpoints have been transformed.")
  }
  else {
    out <- cbind(pe, se, lw, up)
  }
  colnames(out) <- c("estimate", "se", "lower", "upper")
  rownames(out) <- " "
  return(out)
}
