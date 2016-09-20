#' Optimum allocation for stratified random sampling.
#' 
#' This is a function to compute the optimum allocation for a stratified random sampling design. 
#' 
#' @param Ni Vector of total number of sampling units in each stratum.
#' @param si Vector of the standard deviation for each statum
#' @param ci Vector of the cost per unit for each stratum.
#' @param c0 Overhead survey cost.
#' @param ct Total cost for the survey.
#' @param ev Variance for the estimator of the population mean.
#' 
#' @details The solution is based choosing the overall sample size and the sample sizes for each stratum to minimize the estimator variance for a fixed cost, or to minimize the cost for a fixed estimator variance. The solution assumes the usual estimator for the for the population mean and a variance based on a finite sample design-based approach. The total cost of the survey is assumed to be the overhead cost plus a per-unit cost for each sampled unit. See Cochran (1977) for details. 
#' 
#' @source Cochran, W. G. (1977). \emph{Sampling techniques} (3rd Edition). New York: Wiley. 
#' 
#' @return A list of a vector of the proportion of units to sample from each stratum, a vector of sample sizes for each stratum, total sample size, estimator variance, and total survey cost. If neither the estimator variance (\code{ev}) or the total cost (\code{ct}) are specified then only the proportions can be computed.  
#' 
#' @examples 
#' # sampling fractions only
#' allocate(Ni = c(155,62,93), si = c(5,15,10), ci = c(9,9,16))
#' 
#' # allocation for estimator variance fixed at 1 
#' allocate(Ni = c(155,62,93), si = c(5,15,10), ci = c(9,9,16), ev = 1)
#' 
#' # allocation for total survey cost fixed at 500
#' allocate(Ni = c(155,62,93), si = c(5,15,10), ci = c(9,9,16), ct = 500)
#' @export
allocate <- function(Ni, si, ci = rep(1, length(Ni)), c0 = 0, ct = NA, ev = NA) {
  f <- Ni*si/sqrt(ci)/sum(Ni*si/sqrt(ci))
  N <- sum(Ni)
  if (!is.na(ct) & !is.na(ev)) {
    stop("survey cost and variance cannot both be fixed")
  }
  else if (is.na(ct) & is.na(ev)) {
    return(list(fractions = f, ni = NA, variance = NA, cost = NA))  
  }
  else {
    t1 <- sum(Ni*si/sqrt(ci))
    t2 <- sum(Ni*si*sqrt(ci))
    if (!is.na(ev)) {
      n <- t1 * t2/(ev * N^2 + sum(Ni*si^2))
      ni <- n * f
      if (any(ni > Ni)) warning("optimum sample size exceeds available units")
      return(list(fractions = f, ni = n*f, n = n, 
        variance = ifelse(all(ni <= Ni), sum(Ni^2 * (Ni-ni)/Ni * (si^2/ni))/N^2, NA),
        cost = ifelse(all(ni <= Ni), c0 + sum(ni*ci), NA)))
    }  
    if (!is.na(ct)) {
      n <- (ct - c0) * t1/t2
      ni <- n * f
      if (any(ni > Ni)) warning("optimum sample size exceeds available units")
      return(list(fractions = f, ni = n*f, n = n, 
        variance = ifelse(all(ni <= Ni), sum(Ni^2 * (Ni-ni)/Ni * (si^2/ni))/N^2, NA),
        cost = ifelse(all(ni <= Ni), c0 + sum(ni*ci), NA)))
    }
  }
}