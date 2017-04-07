#' Delta method for model objects (experimental).
#' 
#' This function applies the "delta method" to arbitrary model objects provided that one can extract the parameter estimates and the (estimated) covariance matrix of the estimator from the object. Derivatives are computed using numerical differentiation. An alternative simulation-based method is also possilble.   
#' 
#' @param object Model object. Just about any object can be specified provided that functions can also be specified to extract the parameter estimates and the estimated covariance matrix of the estimators.
#' @param pfunc Character object that is function of the parameters that can be evaluated by R. Parameters must be referenced by name as specified by \code{pname}.
#' @param pname Names of the parameters extracted by \code{cfunc}.
#' @param cfunc Function for extracting the parameter estimates from \code{object} (default is \code{coef}).
#' @param vfunc Function for extracting the estimated covariance matrix of the estimator from \code{object} (default is \code{vcov}).
#' @param B Number of bootstrap samples. The default (\code{B=0}) results in numerical differentiation instead. 
#' @param level Confidence level in (0,1) (default is 0.95). 
#' @param ... Optional arguments for \code{jacobian}. 
#' 
#' @details By default the function applies the delta method using numerical differentiation. However if \code{B} > 0 then a simulation-based bootstrap method described by Mandel (2013) which avoids needing to compute derivatives. Note that both methods are based on asymptotic arguments. 
#' 
#' @source Mandel, M. (2013). Simulation-based confidence intervals for functions with complicated derivatives. \emph{The American Statistician}, \emph{62}, 76-81.
#' 
#' @importFrom numDeriv jacobian
#' @importFrom MASS mvrnorm
#' @export
dmethod <- function(object, pfunc, pname, cfunc = coef, vfunc = vcov, B = 0, level = 0.95, ...) {
  f <- function(theta, pfunc, pname) {
    theta <- as.list(theta)
    names(theta) <- pname
    return(with(theta, eval(parse(text = pfunc))))
  }
  pe <- f(cfunc(object), pfunc, pname)
  if (B == 0) {
    gr <- numDeriv::jacobian(f, cfunc(object), pfunc = pfunc, pname = pname, ...)
    va <- diag(gr %*% vfunc(object) %*% t(gr))
  }
  else {
    y <- data.frame(MASS::mvrnorm(B, cfunc(object), vfunc(object)))
    colnames(y) <- pname
    y <- lapply(split(y, seq(B)), function(z) with(z, eval(parse(text = pfunc))))
    va <- apply(sweep(do.call("rbind", y), 2, pe), 2, function(z) sum(z^2)/B)
  }  
  lw <- pe - qnorm(level + (1 - level)/2) * sqrt(va)
  up <- pe + qnorm(level + (1 - level)/2) * sqrt(va)
  out <- data.frame(estimate = pe, se = sqrt(va), lower = lw, upper = up)
  return(out)
}

dmethod(model, myfunc, mynames, cfunc = cfunc, B = 1000)
dmethod(model, myfunc, mynames, cfunc = cfunc)
