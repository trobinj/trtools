#' Confidence intervals for the estimated expected response of generalized linear models (experimental).
#' 
#' This function computes the estimated expected response and the confidence interval for the estimated expected response for (generalized) linear models. 
#' 
#' @param object Model object of class \code{glm}.
#' @param newdata An optional data frame in which to look for variables with which to predict. If omitted the data frame used in creating the model object is used. 
#' @param df Degrees of freedom for the confidence interval. If omitted it is extracted from the model object.
#' @param level Confidence level in (0,1).
#' 
#' @details This function simplifies the process of computing the endpoints of the confidence interval for the expected response on the scale of the response variable for generalized linear models using the \code{predict} function with \code{se.fit = TRUE}. It computes the Wald confidence interval on the scale of the linear predictor and then uses the inverse of the link function to map the interval to the scale of the response variable. 
#' 
#' @examples 
#' m <- glm(cbind(y,total-y) ~ density + species, family = quasibinomial, data = rotifer)
#' d <- expand.grid(density = seq(1.01, 1.07, by = 0.01), species = c("kc","pm"))
#' glmint(m, newdata = d)
#' 
#' @importFrom stats predict
#' @export
glmint <- function(object, newdata, df, level = 0.95) {
  if (!(all(class(object) %in% c("lm","glm")))) {
    stop("object not of class lm or glm")
  }
  if (missing(newdata)) {
    tmp <- predict(object, se.fit = TRUE)
  }
  else {
    tmp <- predict(object, newdata = newdata, se.fit = TRUE)
  }
  if (missing(df)) {
    if (("glm" %in% class(object)) && (family(object)[1] %in% c("binomial","poisson"))) {
      df <- Inf
    }
    else {
      df <- summary(object)$df[2]
    }
  }
  fit <- family(object)$linkinv(tmp$fit)
  low <- family(object)$linkinv(tmp$fit - qt((level + 1)/2, df) * tmp$se)
  upp <- family(object)$linkinv(tmp$fit + qt((level + 1)/2, df) * tmp$se)
  out <- data.frame(fit, low, upp)
  return(out)
}