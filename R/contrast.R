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
contrast <- function(model, a, b, u, v, fcov = vcov, level = 0.95, cnames = NULL, ...) {
  if (class(model) != "lm") stop("function currently only works for lm objects")
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
  mm <- ma - mb - mu + mv
  se <- sqrt(diag(mm %*% fcov(model, ...) %*% t(mm)))
  pe <- mm %*% coef(model)
  df <- summary(model)$df[2]
  lw <- pe - qt(level + (1 - level)/2, df) * se
  up <- pe + qt(level + (1 - level)/2, df) * se 
  ts <- pe/se
  pv <- 2*pt(-abs(ts), df)
  out <- cbind(pe, se, lw, up, ts, df, pv)
  colnames(out) <- c("Estimate", "Std. Error", "Lower", "Upper", "t value", "df", "Pr(>|t|)")
  rownames(out) <- cnames
  return(out)
}
