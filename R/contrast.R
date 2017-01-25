#' Inferences for contrasts for linear models (experimental).
#' 
#' This is a function allows one to obtain standard inferences (i.e., point estimates, standard errors, confidence intervals, etc.) concerning one or more contrasts.
#' 
#' @param model Model object. Currently only objects of class \code{lm} are accepted.
#' @param a List or data frame defining a linear combination. 
#' @param b List or data frame defining a linear combination. 
#' @param a2 List or data frame defining a linear combination. 
#' @param b2 List or data frame defining a linear combination.
#' @param level Confidence level in (0,1).
#' @param fcov Function for estimating the variance-covariance matrix of the model parameters.
#' @param cnames Labels for the contrasts.
#' @param ... Arguments to pass to \code{fcov}.
#' 
#' @details Details go here. 
#' 
#' @export
contrast <- function(model, a, b, a2, b2, fcov = vcov, level = 0.95, cnames = NULL, ...) {
  if (class(model) != "lm") stop("function currently only works for lm objects")
  formula.rhs <- as.formula(paste("~", strsplit(as.character(formula(model)), "~")[[3]]))
  dat <- model.matrix(model)
  for (i in names(dat)) {
    if (is.character(dat[[i]])) {
      dat[[i]] <- as.factor(dat[[i]])
    }
  }
  ma1 <- as.data.frame(a)
  tmp <- intersect(names(ma1), names(model.frame(model)))
  for (i in tmp) {
    if (is.factor(model.frame(model)[[i]])) {
      ma1[[i]] <- fct_relevel(ma1[[i]], levels(model.frame(model)[[i]]))
      attributes(ma1[[i]]) <- attributes(model.frame(model)[[i]])
    }
  }
  ma1 <- model.matrix(formula.rhs, ma1)
  if (!missing(b)) {
    mb1 <- as.data.frame(b)
    tmp <- intersect(names(mb1), names(model.frame(model)))
    for (i in tmp) {
      if (is.factor(model.frame(model)[[i]])) {
        mb1[[i]] <- fct_relevel(mb1[[i]], levels(model.frame(model)[[i]]))
        attributes(mb1[[i]]) <- attributes(model.frame(model)[[i]])
      }
    }
    mb1 <- model.matrix(formula.rhs, mb1)
  } 
  else {
    mb1 <- 0
  }
  if (!missing(a2)) {ma2 <- model.matrix(formula.rhs, as.data.frame(a2))} else {ma2 <- 0}
  if (!missing(b2)) {mb2 <- model.matrix(formula.rhs, as.data.frame(b2))} else {mb2 <- 0}
  mm <- ma1 - mb1 - ma2 + mb2
  se <- sqrt(diag(mm %*% fcov(model, ...) %*% t(mm)))
  pe <- mm %*% coef(model)
  df <- summary(model)$df[2]
  lw <- pe - qt(level + (1 - level)/2, df) * se
  up <- pe + qt(level + (1 - level)/2, df) * se 
  ts <- pe/se
  pv <- 2*pt(-abs(ts), df)
  out <- cbind(pe, se, ts, pv, lw, up)
  colnames(out) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)", "lower", "upper")
  rownames(out) <- cnames
  return(out)
}
