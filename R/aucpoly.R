#' Draw/color/shade area under a curve.
#' 
#' This is a utility function mainly for drawing/coloring/shading the area under the curve of a fuction (typically a probability density function).  
#' 
#' @param x Vector of values at which to evaluate the curve.
#' @param f Function for the curve.
#' @param a Lower bound of the interval.
#' @param b Upper bound of the interval.
#' @param ... Additional arguments to polygon. 
#' 
#' @export
aucpoly <- function(x, f, a, b, ...) {
  xtmp <- c(a, a, x[(a <= x) & (x <= b)], b, b)
  ytmp <- c(0, f(a), f(x)[(a <= x) & (x <= b)], f(b), 0)
  polygon(xtmp, ytmp, ...)
}