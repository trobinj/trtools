stratify <- function(x, strata, breaks, plot = TRUE) {
  h <- hist(x, plot = FALSE, breaks = breaks)
  g <- length(h$counts)
  z <- data.frame(
    lower = rep(NA, g),
    upper = rep(NA, g),
    frequency = h$counts,
    sqrtf = sqrt(h$counts),
    csqrtf = cumsum(sqrt(h$counts)),
    stratum = NA)
  k <- 1:(strata - 1) * max(z$csqrtf)/strata
  for (i in 1:g) {
    z$lower[i] <- h$breaks[i]
    z$upper[i] <- h$breaks[i+1]
  }
  for (i in 1:(strata - 1)) {
    tmp <- which(abs(z$csqrtf - k[i]) == min(abs(z$csqrtf - k[i])))
    z$stratum[c(1:g) <= tmp & is.na(z$stratum)] <- i
  }
  z$stratum[is.na(z$stratum)] <- strata
  if (plot) {
    p <- ggplot(data.frame(z, mids = h$mids)) +
      geom_bar(aes(x = mids, y = frequency, fill = factor(stratum)),
        color = "black", stat = "identity", width = (z$upper[1]-z$lower[1])) +
      guides(fill = guide_legend(override.aes = list(colour = NULL), title = "Stratum")) +
      theme_bw() + xlab("Stratification Variable") + ylab("Frequency") +
      theme(rect = element_rect(fill = "transparent", color = NA), panel.background = element_rect(fill = "transparent", color = NA))
    plot(p)
  }
  return(list(output = z, cutpoints = k))
}
