#' Delta method for regression model objects (experimental).
#' 
#' This function applies the "delta method" to functions of parameters of model objects, provided that functions can be specified to extract the parameter estimates and the (estimated) covariance matrix of the estimator from the object. Derivatives are computed using numerical differentiation. Alternatively standard errors can be approximated using a bootstrap approach described by Mandel (2013). It is based on the \code{deltaMethod} function from the **car** package, but uses numerical rather than analytical derivatives, and has a few other options. 
#' 
#' @param object Model object. Just about any object can be specified provided that functions can also be specified to extract the parameter estimates and the estimated covariance matrix of the estimators.
#' @param pfunc A character object that is function of the parameters that can be evaluated by R. Parameters must be referenced by name as specified by \code{pname}. Alternatively a function that takes as an argument the parameters. In either case the function can return a scalar or vector. 
#' @param pname Names of the parameters extracted by \code{cfunc}.
#' @param cfunc Function for extracting the parameter estimates from \code{object} (default is \code{coef}).
#' @param vfunc Function for extracting the estimated covariance matrix of the estimator from \code{object} (default is \code{vcov}).
#' @param tfunc Function for transforming the result of \code{pfunc} for cases when the sampling distribution is thought to be more approximately normal on some other scale (e.g., log).
#' @param fname Character string(s) for function names for output. 
#' @param B Number of bootstrap samples. The default (\code{B = 0}) results in numerical differentiation instead.
#' @param sample Return sample of simulated realizations from sampling distribution when B > 0. 
#' @param level Confidence level in (0,1) (default is 0.95). 
#' @param df Degrees of freedom (infinite by default).
#' @param ... Optional arguments for \code{jacobian}. 
#' 
#' @details By default the function applies the delta method using numerical differentiation. However if \code{B} > 0 then a bootstrap method described by Mandel (2013) is used which avoids needing to compute derivatives.
#' 
#' @source Mandel, M. (2013). Simulation-based confidence intervals for functions with complicated derivatives. *The American Statistician*, *62*, 76-81.
#' 
#' @examples 
#' # estimation of LD50 for each type of insecticide
#' m <- glm(cbind(deaths, total - deaths) ~ insecticide * log2(deposit),
#'   family = binomial, data = insecticide)
#' dmethod(m, "2^c(-b0/b3, -(b0+b1)/(b3+b4), -(b0+b2)/(b3+b5))",
#'   paste("b", 0:5, sep = ""), fname = c("BHC","both","DDT"))
#' 
#' @importFrom numDeriv jacobian
#' @importFrom MASS mvrnorm
#' @importFrom stats qnorm
#' @export
#' @md
dmethod <- function(object, pfunc, pname = NULL, cfunc = coef, vfunc = vcov, tfunc, fname, B = 0, sample = FALSE, level = 0.95, df = Inf, ...) {
  
  if (!is.function(pfunc) & length(cfunc(object)) != length(pname)) {
    stop("parameter names missing")
  }
  
  if (is.function(pfunc)) {
    f <- pfunc
  } else {
    f <- function(theta) {
      theta <- as.list(theta)
      names(theta) <- pname
      return(with(theta, eval(parse(text = pfunc))))
    }
  }
  
  pe <- f(cfunc(object))
  
  if (B == 0) {
    ja <- numDeriv::jacobian(f, cfunc(object), ...)
    va <- diag(ja %*% vfunc(object) %*% t(ja))
  }
  else {
    y <- apply(MASS::mvrnorm(B, cfunc(object), vfunc(object)), 1, f)
    if (is.vector(y)) {
      y <- as.matrix(y)
    } else {
      y <- t(y)
    }
    va <- apply(sweep(y, 2, pe), 2, function(z) sum(z^2) / B)
  }  
  
  se <- sqrt(va)
  lw <- pe - qt(level + (1 - level)/2, df = df) * se
  up <- pe + qt(level + (1 - level)/2, df = df) * se
  ts <- pe/se
  pv <- 2*pt(-abs(ts), df)
  if (!missing(tfunc)) {
    if (any(tfunc(lw) > tfunc(up))) {
      tmp <- lw
      lw <- up
      up <- tmp
    }
    out <- cbind(tfunc(pe), tfunc(lw), tfunc(up))
    colnames(out) <- c("estimate", "lower", "upper")
  }
  else {
    out <- cbind(pe, se, lw, up, ts, df, pv)
    colnames(out) <- c("estimate", "se", "lower", "upper", "tvalue", "df", "pvalue")
  }
  if (missing(fname)) {
    rownames(out) <- rep("", nrow(out))
  }
  else {
    fname <- as.character(fname)
    rownames(out) <- fname
    if (sample) {
      colnames(y) <- fname
    }
  }
  if (B > 0 & sample) {
    return(list(estimates = out, sample = y))
  }
  else return(out)
}

