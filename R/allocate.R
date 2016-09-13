allocate <- function(Ni, si, ci = rep(1, length(Ni)), c0 = 0, ct = NA, vt = NA) {
  f <- Ni*si/sqrt(ci)/sum(Ni*si/sqrt(ci))
  N <- sum(Ni)
  if (!is.na(ct) & !is.na(vt)) {
    stop("survey cost and variance cannot both be fixed")
  }
  else if (is.na(ct) & is.na(vt)) {
    return(list(fractions = f, ni = NA, variance = NA, cost = NA))  
  }
  else {
    t1 <- sum(Ni*si/sqrt(ci))
    t2 <- sum(Ni*si*sqrt(ci))
    if (!is.na(vt)) {
      n <- t1 * t2/(vt * N^2 + sum(Ni*si^2))
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