#' Cumulative square root frequency stratification.
#' 
#' This function implements the "cumulative square root frequency method" (Dalenius & Hodges, 1959) for determining the approximately optimal stratification of elements for stratified random sampling with Neyman allocation. 
#' 
#' @param x An auxiliary variable to be used for stratification.
#' @param strata Number of strata.
#' @param breaks Breaks for the auxiliary variable expressed as a vector of cut points. 
#'
#' @details See Dalenius and Hodges (1959) or Cochran (1977) for details. Ideally the auxiliary variable should be strongly correlated with the target variable. 
#' 
#' @source Cochran, W. G. (1977). \emph{Sampling techniques} (3rd Edition). New York: Wiley.
#' 
#' Dalenius, T. & Hodges, J. L. Jr. (1959). Minimum variance stratification. \emph{Journal of the American Statistical Assocation}, \emph{54}, 88-101.
#' 
#' @return A list object including a data frame giving the strata assignment of the elements and the cut points that define the strata in terms of the auxiliary variable. 
#' 
#' @examples 
#' # replication of an example from Cochran (1977)
#' x <- rep(seq(2.5, 97.5, by = 5), c(3464, 2516, 2157, 1581, 1142, 
#'   746, 512, 376, 265, 207, 126, 107, 82, 50, 39, 25, 16, 19, 2, 3))
#' stratify(x, strata = 5, breaks = seq(0, 100, by = 5))
#' # artificial data with a normally-distributed auxiliary variable
#' set.seed(101)
#' x <- rnorm(10000, 20, 3)
#' stratify(x, strata = 4, breaks = 25)
#' @importFrom graphics hist
#' @export
stratify <- function(x, strata, breaks) {
h <- hist(x, plot = FALSE, breaks = breaks)
g <- length(h$counts)
z <- data.frame(
  lower = rep(NA, g),
  upper = rep(NA, g),
  freq = h$counts,
  sqrtf = sqrt(h$counts),
  csqrtf = cumsum(sqrt(h$counts)),
  stratum = NA)
k <- 1:(strata - 1) * max(z$csqrtf)/strata
for (i in 1:g) {
  z$lower[i] <- h$breaks[i]
  z$upper[i] <- h$breaks[i + 1]
}
for (i in 1:(strata - 1)) {
  tmp <- which(abs(z$csqrtf - k[i]) == min(abs(z$csqrtf - k[i])))
  z$stratum[c(1:g) <= tmp & is.na(z$stratum)] <- i
}
z$stratum[is.na(z$stratum)] <- strata
return(list(output = z, cutpoints = k))
}