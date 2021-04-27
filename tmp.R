tmp <- function(model, a, b, u, v, df, tf, cnames, level = 0.95, fcov = vcov, delta = FALSE, adjust = FALSE, ...) {
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
    ve <- gr %*% mm %*% fcov(model) %*% t(mm) %*% t(gr)
    se <- sqrt(diag(ve))
    pe <- tf(pe)
  } else {
    ve <- mm %*% fcov(model) %*% t(mm)
    se <- sqrt(diag(ve))
  }
  
  if (adjust) {
    
    ts <- pe/se
    pv <- rep(NA, length(ts))
    
    for (i in 1:length(pv)) {
      pv[i] <- 1 - mvtnorm::pmvt(
        lower = rep(-abs(ts[i]), length(ts)),
        upper = rep( abs(ts[i]), length(ts)),
        df = df, corr = cov2cor(ve), ...)
    }

    ca <- mvtnorm::qmvt(level, df = df, corr = cov2cor(ve), tail = "both.tails", ...)$quantile
    
    lw <- pe - ca * se
    up <- pe + ca * se
    
  } else {

    ts <- pe/se
    pv <- 2*pt(-abs(ts), df)
          
    lw <- pe - qt(level + (1 - level)/2, df) * se
    up <- pe + qt(level + (1 - level)/2, df) * se
  }
  
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

n <- 5
d <- expand.grid(n = 1:n, layer = unique(trtools::daphniastrat$layer))
d$count <- rpois(nrow(d), 10)

d <- trtools::daphniastrat

m <- lm(count ~ layer, data = d)

library(emmeans)
emm_options(emmeans = list(infer = c(FALSE,TRUE)), contrast = list(infer = c(FALSE,TRUE)))

set.seed(124)
ugh <- pairs(emmeans(m, ~layer), adjust = "mvt")
set.seed(124)
print(ugh)$p.value


set.seed(124)
tmp(m, a = list(layer = c("epilimnion","epilimnion","thermocline")),
    b = list(layer = c("thermocline","hypolimnion","hypolimnion")), adjust = TRUE)


