#' Inferences for contrasts for linear models (experimental).
#' 
#' This is a function allows one to obtain standard inferences (i.e., point estimates, standard errors, confidence intervals, etc.) concerning one or more contrasts expressed in terms of (differences among) linear combinations of the parameters. 
#' 
#' @param model Model object. Currently only objects of class \code{lm} are accepted.
#' @param a List or data frame defining a linear combination. 
#' @param b List or data frame defining a linear combination. 
#' @param u List or data frame defining a linear combination. 
#' @param v List or data frame defining a linear combination.
#' @param df Optional degrees of freedom. If left missing the residual degrees of freedom will be used except for GLMs with \code{family = poisson} or \code{family = binomial} in which case an infinite degrees of freedom is used.  
#' @param tf Optional transformation function to apply to the point estimate(s) and confidence interval limits (e.g., \code{tf = exp} for a logistic model to estimate odds or odds ratios). 
#' @param cnames Labels for the contrasts.
#' @param level Confidence level in (0,1).
#' @param fcov Function for estimating the variance-covariance matrix of the model parameters.
#' @param ... Arguments to pass to \code{fcov}.
#' 
#' @details Assuming a (generalized) linear model of the for \eqn{g[E(Y_i)] = \eta_i} where \eqn{\eta_i = \beta_0 + \beta_1 x_{i1} + \beta_2 x_{i2} + \dots + \beta_p x_{ip}}, many contrasts or linear combinations of the parameters can be written in the form \eqn{\eta_a - \eta_b - (\eta_u - \eta_v)} where the subscripts represent specified values of \eqn{x_{i1}, x_{i2}, \dots, x_{ip}}. The arguments a, b, u, and v correspond to these specified values, where a value of zero is assumed by default if one or more of these are not specified. 
#' 
#' @importFrom stats formula as.formula model.frame model.matrix pt
#' @export
contrast <- function(model, a, b, u, v, df, tf, cnames, level = 0.95, fcov = vcov, ...) {
  if (!any(class(model) %in% c("lm","glm"))) {
    stop("function currently only works for lm and glm objects")
  }
  formula.rhs <- as.formula(paste("~", strsplit(as.character(formula(model)), "~")[[3]]))
  dat <- model.frame(model)
  for (i in names(dat)) {
    if (is.character(dat[[i]])) {
      dat[[i]] <- as.factor(dat[[i]])
    }
  }
  if (all(missing(a), missing(b), missing(u), missing(v))) {
    stop("no contrast(s) specified")
  }
  foo <- function(mat, dat) {
    mat <- as.data.frame(mat)
    tmp <- intersect(names(mat), names(dat))
    if (length(setdiff(names(mat), names(dat))) > 0) {
      stop(paste("variable(s)", setdiff(names(mat), names(dat)), "not found"))
    }
    for (i in tmp) {
      if (is.factor(dat[[i]])) {
        if (length(setdiff(levels(mat[[i]]), levels(dat[[i]]))) > 0) {
          stop(paste("level(s)", setdiff(levels(mat[[i]]), levels(dat[[i]])), "not found"))
        }
        mat[[i]] <- factor(mat[[i]], levels = levels(dat[[i]]))
        attributes(mat[[i]]) <- attributes(dat[[i]])
      }
    }
    return(model.matrix(formula.rhs, mat))
  }
  if (!missing(a)) {
    ma <- foo(a, dat)
  }
  else {
    ma <- 0
  }
  if (!missing(b)) {
    mb <- foo(b, dat)
  }
  else {
    mb <- 0
  }
  if (!missing(u)) {
    mu <- foo(u, dat)
  }
  else {
    mu <- 0
  }
  if (!missing(v)) {
    mv <- foo(v, dat)
  }
  else {
    mv <- 0
  }
  rowmax <- max(unlist(lapply(list(ma, mb, mu, mv), function(x) ifelse(is.matrix(x), nrow(x), 0))))
  if (is.matrix(ma) && nrow(ma) == 1) {
    ma <- ma[rep(1, rowmax),]
  }
  if (is.matrix(mb) && nrow(mb) == 1) {
    mb <- mb[rep(1, rowmax),]
  }
  if (is.matrix(mu) && nrow(mu) == 1) {
    mu <- mu[rep(1, rowmax),]
  }
  if (is.matrix(mv) && nrow(mv) == 1) {
    mv <- mv[rep(1, rowmax),]
  }
  mm <- ma - mb - mu + mv
  se <- sqrt(diag(mm %*% fcov(model, ...) %*% t(mm)))
  pe <- mm %*% coef(model)
  if (missing(df)) {
    if (("glm" %in% class(model)) && (family(model)[1] %in% c("binomial","poisson"))) {
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
    out <- cbind(tf(pe), se, tf(lw), tf(up), ts, df, pv)
    colnames(out) <- c("f(Estimate)", "SE", "f(Lower)", "f(Upper)", "t value", "df", "Pr(>|t|)")
  }
  else {
    out <- cbind(pe, se, lw, up, ts, df, pv)
    colnames(out) <- c("Estimate", "SE", "Lower", "Upper", "t value", "df", "Pr(>|t|)")
  }
  if (missing(cnames)) {
    rownames(out) <- rep("", nrow(out))
  }
  else {
    rownames(out) <- cnames
  }
  return(out)
}
