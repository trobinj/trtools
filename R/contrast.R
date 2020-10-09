#' Inferences for contrasts for (generalized) linear models (experimental).
#' 
#' This is a function allows one to obtain standard inferences (i.e., point estimates, standard errors, confidence intervals, etc.) concerning one or more contrasts expressed in terms of (differences among) linear combinations of the parameters. 
#' 
#' @aliases contrast.lm contrast.glm contrast.lmerMod contrast.glmerMod contrast.gls
#' 
#' @usage
#' \method{contrast}{lm}(model, a, b, u, v, df, tf, cnames, level = 0.95, fcov = vcov, delta = FALSE, ...)
#' \method{contrast}{glm}(model, a, b, u, v, df, tf, cnames, level = 0.95, fcov = vcov, delta = FALSE, ...)
#' \method{contrast}{lmerMod}(model, a, b, u, v, df, tf, cnames, level = 0.95, fcov = vcov, ...)
#' \method{contrast}{glmerMod}(model, a, b, u, v, df, tf, cnames, level = 0.95, fcov = vcov, ...)
#' \method{contrast}{gls}(model, a, b, u, v, df, tf, cnames, level = 0.95, fcov = vcov, ...)
#' 
#' @param model Model object.
#' @param a List or data frame defining a linear combination. 
#' @param b List or data frame defining a linear combination. 
#' @param u List or data frame defining a linear combination. 
#' @param v List or data frame defining a linear combination.
#' @param df Optional manual specification of the degrees of freedom. This defaults to the residual degrees of freedom for linear and generalized linear models, except when \code{family = binomial} or \code{family = poisson} where the degrees of freedom is infinite. Linear and generalized linear mixed models also use infinite degrees of freedom by default. 
#' @param tf Optional transformation function to apply to the estimates. If \code{delta = FALSE} and an elementwise function is detected, then it is applied to the estimate(s) and confidence interval(s) and only those are reported. But if \code{delta = TRUE} or the function is not elementwise, the delta method is used to produce standard errors as well as Wald confidence intervals and tests. Detection of an elementwise function is done numerically so it may not be completely reliable. 
#' @param cnames Labels for the contrasts. Default is none.
#' @param level Confidence level in (0,1). Default is 95\%.
#' @param fcov Function for estimating the variance-covariance matrix of the model parameters. Default is what is returned by the function \code{vcov}.
#' @param delta Logical for if the delta method should be used with a transformation function (if provided). Default is \code{FALSE}.
#' @param ... Not used.
#' 
#' @details Assuming a (generalized) linear (mixed) model of the for \eqn{g[E(Y_i)] = \eta_i} where \eqn{\eta_i = \beta_0 + \beta_1 x_{i1} + \beta_2 x_{i2} + \dots + \beta_p x_{ip}}, many contrasts or linear combinations of the parameters can be written in the form \eqn{\eta_a - \eta_b - (\eta_u - \eta_v)} where the subscripts represent specified values of \eqn{x_{i1}, x_{i2}, \dots, x_{ip}}. The arguments a, b, u, and v correspond to these specified values, where a value of zero is assumed by default if one or more of these are not specified. Note that for (generalized) linear mixed model, the expectation is computed conditional on setting all random effects equal to zero. 
#' 
#' @examples
#' m <- lm(Gas ~ Temp * Insul, data = MASS::whiteside)
#' # estimate expected gas consumption at zero degrees 
#' # before and after insulation was added
#' contrast(m, a = list(Temp = 0, Insul = c("Before","After")),
#'  cnames = c("Before at 0","After at 0"))
#' # estimate slopes of regression of gas consultion on
#' # before and after insulation was added
#' contrast(m,
#'  a = list(Temp = 1, Insul = c("Before","After")),
#'  b = list(Temp = 0, Insul = c("Before","After")),
#'  cnames = c("Slope Before","Slope After"))
#' # estimate difference in expected gas consumption between
#' # before and after insulation at different temperatures
#' contrast(m,
#'  a = list(Temp = c(0,5,10), Insul = "Before"),
#'  b = list(Temp = c(0,5,10), Insul = "After"),
#'  cnames = c("at 5","at 5","at 10"))
#'
#' m <- glm(cbind(deaths,total-deaths) ~ insecticide * log2(deposit),
#'  family = binomial, data = insecticide)
#' # estimate odds ratios for the effect of doubling the 
#' # deposit for each insecticide
#' types <- levels(insecticide$insecticide)
#' contrast(m,
#'  a = list(insecticide = types, deposit = 2),
#'  b = list(insecticide = types, deposit = 1),
#'  cnames = types, tf = exp)
#' # odds ratios to compare insecticides at a deposit of 5
#' contrast(m,
#'  a = list(insecticide = "both", deposit = 5),
#'  b = list(insecticide = c("DDT","g-BHC"), deposit = 5),
#'  cnames = c("both vs DDT","both vs g-BHC"), tf = exp)
#' # estimate response probabilities for each insecticide
#' # at a deposit level of 5
#' contrast(m, a = list(insecticide = types, deposit = 5),
#'  cnames = types, tf = plogis)
#' 
#' @importFrom stats formula as.formula model.frame model.matrix pt
#' @import lme4
#' @export
contrast <- function(model, ...) {
  UseMethod("contrast", model)
}
#' @export 
contrast.lm <- function(model, a, b, u, v, df, tf, cnames, level = 0.95, fcov = vcov, delta = FALSE, ...) {
  if (all(missing(a), missing(b), missing(u), missing(v))) {
    stop("no contrast(s) specified")
  }
  eta <- function(theta, model, data) {
    model$coefficients <- theta
    predict(model, as.data.frame(data))
  }
  if (!missing(a)) {
    ma <- numDeriv::jacobian(eta, coef(model), model = model, data = a)
    pa <- predict(model, as.data.frame(a))
  }
  else {
    ma <- 0
    pa <- 0
  }
  if (!missing(b)) {
    mb <- numDeriv::jacobian(eta, coef(model), model = model, data = b)
    pb <- predict(model, as.data.frame(b))
  }
  else {
    mb <- 0
    pb <- 0
  }
  if (!missing(u)) {
    mu <- numDeriv::jacobian(eta, coef(model), model = model, data = u)
    pu <- predict(model, as.data.frame(u))
  }
  else {
    mu <- 0
    pu <- 0
  }
  if (!missing(v)) {
    mv <- numDeriv::jacobian(eta, coef(model), model = model, data = v)
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
  
  pe <- pa - pb - pu + pv
  
  if (missing(df)) {
    df <- summary(model)$df[2]
  }

  if (!missing(tf)) {
    gr <- numDeriv::jacobian(tf, pe) 
    if (any(diag(nrow(gr)) != (gr != 0) %*% t(gr != 0))) {
      delta <- TRUE
    }
  }
  if (delta & !missing(tf)) {
    se <- sqrt(diag(gr %*% mm %*% fcov(model) %*% t(mm) %*% t(gr)))
    pe <- tf(pe)
  } else {
    se <- sqrt(diag(mm %*% fcov(model) %*% t(mm)))
  }

  lw <- pe - qt(level + (1 - level)/2, df) * se
  up <- pe + qt(level + (1 - level)/2, df) * se
  
  ts <- pe/se
  pv <- 2*pt(-abs(ts), df)
  
  if (missing(tf) | delta) {
    out <- cbind(pe, se, lw, up, ts, df, pv)
    colnames(out) <- c("estimate", "se", "lower", "upper", "tvalue", "df", "pvalue")
  } else {
    out <- cbind(tf(pe), tf(lw), tf(up))
    for (i in 1:nrow(out)) {
      if (out[i,2] > out[i,3]) {
        out[i,2:3] <- out[i,3:2] 
      }
    }
    colnames(out) <- c("estimate", "lower", "upper")
  }

  if (missing(cnames)) {
    rownames(out) <- rep("", nrow(out))
  }
  else {
    rownames(out) <- as.character(cnames)
  }
  return(out)
}
#' @export 
contrast.gls <- function(model, a, b, u, v, df, tf, cnames, level = 0.95, fcov = vcov, delta = FALSE, ...) {
  if (all(missing(a), missing(b), missing(u), missing(v))) {
    stop("no contrast(s) specified")
  }
  eta <- function(theta, model, data) {
    model$coefficients <- theta
    predict(model, as.data.frame(data))
  }
  if (!missing(a)) {
    ma <- numDeriv::jacobian(eta, coef(model), model = model, data = a)
    pa <- predict(model, as.data.frame(a))
  }
  else {
    ma <- 0
    pa <- 0
  }
  if (!missing(b)) {
    mb <- numDeriv::jacobian(eta, coef(model), model = model, data = b)
    pb <- predict(model, as.data.frame(b))
  }
  else {
    mb <- 0
    pb <- 0
  }
  if (!missing(u)) {
    mu <- numDeriv::jacobian(eta, coef(model), model = model, data = u)
    pu <- predict(model, as.data.frame(u))
  }
  else {
    mu <- 0
    pu <- 0
  }
  if (!missing(v)) {
    mv <- numDeriv::jacobian(eta, coef(model), model = model, data = v)
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
  
  if (missing(df)) {
    df <- summary(model)$dims$N - summary(model)$dims$p # not sure about this in general
  }
  
  pe <- pa - pb - pu + pv
  
  if (!missing(tf)) {
    gr <- numDeriv::jacobian(tf, pe) 
    if (any(diag(nrow(gr)) != (gr != 0) %*% t(gr != 0))) {
      delta <- TRUE
    }
  }
  if (delta & !missing(tf)) {
    se <- sqrt(diag(gr %*% mm %*% fcov(model) %*% t(mm) %*% t(gr)))
    pe <- tf(pe)
  } else {
    se <- sqrt(diag(mm %*% fcov(model) %*% t(mm)))
  }
  
  lw <- pe - qt(level + (1 - level)/2, df) * se
  up <- pe + qt(level + (1 - level)/2, df) * se
  
  ts <- pe/se
  pv <- 2*pt(-abs(ts), df)
  
  if (missing(tf) | delta) {
    out <- cbind(pe, se, lw, up, ts, df, pv)
    colnames(out) <- c("estimate", "se", "lower", "upper", "tvalue", "df", "pvalue")
  } else {
    out <- cbind(tf(pe), tf(lw), tf(up))
    for (i in 1:nrow(out)) {
      if (out[i,2] > out[i,3]) {
        out[i,2:3] <- out[i,3:2] 
      }
    }
    colnames(out) <- c("estimate", "lower", "upper")
  }
  
  
  # se <- sqrt(diag(mm %*% fcov(model) %*% t(mm)))
  # pe <- pa - pb - pu + pv
  # if (missing(df)) {
  #   df <- summary(model)$dims$N - summary(model)$dims$p # not sure about this in general
  # }
  # lw <- pe - qt(level + (1 - level)/2, df) * se
  # up <- pe + qt(level + (1 - level)/2, df) * se 
  # ts <- pe/se
  # pv <- 2*pt(-abs(ts), df)
  # if (!missing(tf)) {
  #   if (any(tf(lw) > tf(up))) {
  #     tmp <- lw
  #     lw <- up
  #     up <- tmp
  #   }
  #   out <- cbind(tf(pe), tf(lw), tf(up))
  #   colnames(out) <- c("estimate", "lower", "upper")
  # }
  # else {
  #   out <- cbind(pe, se, lw, up, ts, df, pv)
  #   colnames(out) <- c("estimate", "se", "lower", "upper", "tvalue", "df", "pvalue")
  # }
  
  if (missing(cnames)) {
    rownames(out) <- rep("", nrow(out))
  }
  else {
    rownames(out) <- as.character(cnames)
  }
  return(out)
}
#' @export 
contrast.glm <- function(model, a, b, u, v, df, tf, cnames, level = 0.95, fcov = vcov, delta = FALSE, ...) {
  if (all(missing(a), missing(b), missing(u), missing(v))) {
    stop("no contrast(s) specified")
  }
  eta <- function(theta, model, data) {
    model$coefficients <- theta
    predict(model, as.data.frame(data))
  }
  if (!missing(a)) {
    ma <- numDeriv::jacobian(eta, coef(model), model = model, data = a)
    pa <- predict(model, as.data.frame(a))
  }
  else {
    ma <- 0
    pa <- 0
  }
  if (!missing(b)) {
    mb <- numDeriv::jacobian(eta, coef(model), model = model, data = b)
    pb <- predict(model, as.data.frame(b))
  }
  else {
    mb <- 0
    pb <- 0
  }
  if (!missing(u)) {
    mu <- numDeriv::jacobian(eta, coef(model), model = model, data = u)
    pu <- predict(model, as.data.frame(u))
  }
  else {
    mu <- 0
    pu <- 0
  }
  if (!missing(v)) {
    mv <- numDeriv::jacobian(eta, coef(model), model = model, data = v)
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
  
  pe <- pa - pb - pu + pv
  
  if (missing(df)) {
    if (family(model)[1] %in% c("binomial","poisson")) {
      df <- Inf
    }
    else {
      df <- summary(model)$df[2]
    }
  }
  
  if (!missing(tf)) {
    gr <- numDeriv::jacobian(tf, pe) 
    if (any(diag(nrow(gr)) != (gr != 0) %*% t(gr != 0))) {
      delta <- TRUE
    }
  }
  if (delta & !missing(tf)) {
    se <- sqrt(diag(gr %*% mm %*% fcov(model) %*% t(mm) %*% t(gr)))
    pe <- tf(pe)
  } else {
    se <- sqrt(diag(mm %*% fcov(model) %*% t(mm)))
  }
  
  lw <- pe - qt(level + (1 - level)/2, df) * se
  up <- pe + qt(level + (1 - level)/2, df) * se
  
  ts <- pe/se
  pv <- 2*pt(-abs(ts), df)
  
  if (missing(tf) | delta) {
    out <- cbind(pe, se, lw, up, ts, df, pv)
    colnames(out) <- c("estimate", "se", "lower", "upper", "tvalue", "df", "pvalue")
  } else {
    out <- cbind(tf(pe), tf(lw), tf(up))
    for (i in 1:nrow(out)) {
      if (out[i,2] > out[i,3]) {
        out[i,2:3] <- out[i,3:2] 
      }
    }
    colnames(out) <- c("estimate", "lower", "upper")
  }
  
  # se <- sqrt(diag(mm %*% fcov(model) %*% t(mm)))
  # 
  # lw <- pe - qt(level + (1 - level)/2, df) * se
  # up <- pe + qt(level + (1 - level)/2, df) * se 
  # ts <- pe/se
  # pv <- 2*pt(-abs(ts), df)
  # if (!missing(tf)) {
  #   if (any(tf(lw) > tf(up))) {
  #     tmp <- lw
  #     lw <- up
  #     up <- tmp
  #   }
  #   out <- cbind(tf(pe), tf(lw), tf(up))
  #   colnames(out) <- c("estimate", "lower", "upper")
  # }
  # else {
  #   out <- cbind(pe, se, lw, up, ts, df, pv)
  #   colnames(out) <- c("estimate", "se", "lower", "upper", "tvalue", "df", "pvalue")
  # }
  
  if (missing(cnames)) {
    rownames(out) <- rep("", nrow(out))
  }
  else {
    rownames(out) <- as.character(cnames)
  }
  return(out)
}
#' @export 
contrast.lmerMod <- function(model, a, b, u, v, df, tf, cnames, level = 0.95, fcov = vcov, avg = FALSE, ...) {
  if (all(missing(a), missing(b), missing(u), missing(v))) {
    stop("no contrast(s) specified")
  }
  eta <- function(theta, model, data) {
    attr(model, "beta") <- theta
    predict(model, as.data.frame(data), re.form = NA)
  }
  if (!missing(a)) {
    ma <- numDeriv::jacobian(eta, fixef(model), model = model, data = a)
    pa <- predict(model, as.data.frame(a), re.form = NA)
  }
  else {
    ma <- 0
    pa <- 0
  }
  if (!missing(b)) {
    mb <- numDeriv::jacobian(eta, fixef(model), model = model, data = b)
    pb <- predict(model, as.data.frame(b), re.form = NA)
  }
  else {
    mb <- 0
    pb <- 0
  }
  if (!missing(u)) {
    mu <- numDeriv::jacobian(eta, fixef(model), model = model, data = u)
    pu <- predict(model, as.data.frame(u), re.form = NA)
  }
  else {
    mu <- 0
    pu <- 0
  }
  if (!missing(v)) {
    mv <- numDeriv::jacobian(eta, fixef(model), model = model, data = v)
    pv <- predict(model, as.data.frame(v), re.form = NA)
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
  if (avg) {
    tmp <- mm %*% as.matrix(fcov(model)) %*% t(mm)
    tmp <- matrix(1/nrow(tmp), 1, nrow(tmp)) %*% tmp %*% matrix(1/nrow(tmp), nrow(tmp), 1)
    se <- sqrt(diag(tmp))
    pe <- mean(pa - pb - pu + pv)
  }
  else {
    se <- sqrt(diag(mm %*% as.matrix(fcov(model)) %*% t(mm)))
    pe <- pa - pb - pu + pv
  }
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
    rownames(out) <- rep("", nrow(out))
  }
  else {
    rownames(out) <- as.character(cnames)
  }
  return(out)
}
#' @export 
contrast.glmerMod <- function(model, a, b, u, v, df, tf, cnames, level = 0.95, fcov = vcov, ...) {
  if (all(missing(a), missing(b), missing(u), missing(v))) {
    stop("no contrast(s) specified")
  }
  eta <- function(theta, model, data) {
    attr(model, "beta") <- theta
    predict(model, as.data.frame(data), re.form = NA)
  }
  if (!missing(a)) {
    ma <- numDeriv::jacobian(eta, fixef(model), model = model, data = a)
    pa <- predict(model, as.data.frame(a), re.form = NA)
  }
  else {
    ma <- 0
    pa <- 0
  }
  if (!missing(b)) {
    mb <- numDeriv::jacobian(eta, fixef(model), model = model, data = b)
    pb <- predict(model, as.data.frame(b), re.form = NA)
  }
  else {
    mb <- 0
    pb <- 0
  }
  if (!missing(u)) {
    mu <- numDeriv::jacobian(eta, fixef(model), model = model, data = u)
    pu <- predict(model, as.data.frame(u), re.form = NA)
  }
  else {
    mu <- 0
    pu <- 0
  }
  if (!missing(v)) {
    mv <- numDeriv::jacobian(eta, fixef(model), model = model, data = v)
    pv <- predict(model, as.data.frame(v), re.form = NA)
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
  se <- sqrt(diag(mm %*% as.matrix(fcov(model)) %*% t(mm)))
  pe <- pa - pb - pu + pv
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
    rownames(out) <- rep("", nrow(out))
  }
  else {
    rownames(out) <- as.character(cnames)
  }
  return(out)
}
