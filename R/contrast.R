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
  if (all(missing(a), missing(b), missing(u), missing(v))) {
    stop("no contrast(s) specified")
  }
  eta <- function(theta, model, data) {
    model$coefficients <- theta
    predict(model, as.data.frame(data))
  }
  if (!missing(a)) {
    ma <- jacobian(eta, coef(model), model = model, data = a)
    pa <- predict(model, as.data.frame(a))
  }
  else {
    ma <- 0
    pa <- 0
  }
  if (!missing(b)) {
    mb <- jacobian(eta, coef(model), model = model, data = b)
    pb <- predict(model, as.data.frame(b))
  }
  else {
    mb <- 0
    pb <- 0
  }
  if (!missing(u)) {
    mu <- jacobian(eta, coef(model), model = model, data = u)
    pu <- predict(model, as.data.frame(u))
  }
  else {
    mu <- 0
    pu <- 0
  }
  if (!missing(v)) {
    mv <- jacobian(eta, coef(model), model = model, data = v)
    pv <- predict(model, as.data.frame(v))
  }
  else {
    mv <- 0
    pv <- 0
  }
  rowmax <- max(unlist(lapply(list(ma, mb, mu, mv), function(x) ifelse(is.matrix(x), nrow(x), 0))))
  if (is.matrix(ma) && nrow(ma) == 1) {
    ma <- ma[rep(1, rowmax),]
    pa <- pa[rep(1, rowmax)]
  }
  if (is.matrix(mb) && nrow(mb) == 1) {
    mb <- mb[rep(1, rowmax),]
    pb <- pb[rep(1, rowmax)]
  }
  if (is.matrix(mu) && nrow(mu) == 1) {
    mu <- mu[rep(1, rowmax),]
    pu <- pu[rep(1, rowmax)]
  }
  if (is.matrix(mv) && nrow(mv) == 1) {
    mv <- mv[rep(1, rowmax),]
    pv <- pv[rep(1, rowmax)]
  }
  mm <- as.matrix(ma - mb - mu + mv)
  if (ncol(mm) == 1) {
    mm <- t(mm)
  }
  se <- sqrt(diag(mm %*% fcov(model, ...) %*% t(mm)))
  pe <- pa - pb - pu + pv
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
    if (any(tf(lw) > tf(up))) {
      tmp <- lw
      lw <- up
      up <- tmp
    }
    message("Note: Point estimate and confidence interval endpoints have been transformed.")
    out <- cbind(tf(pe), se, tf(lw), tf(up), ts, df, pv)
  }
  else {
    out <- cbind(pe, se, lw, up, ts, df, pv)
  }
  colnames(out) <- c("estimate", "se", "lower", "upper", "tvalue", "df", "pvalue")
  if (missing(cnames)) {
    rownames(out) <- rep("", nrow(out))
  }
  else {
    rownames(out) <- cnames
  }
  return(out)
}
