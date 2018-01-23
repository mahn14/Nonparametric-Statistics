# Finds the confidence interval

## PARAMS ##

# x <- data
# pstar <- quantile
# level <- confidence level

quantile.confint <- function(x, pstar=0.5, level=0.95)
{
  alpha <- 1-level
  n <- length(x)
  xs <- sort(x)
  r <- qbinom(alpha/2, n, pstar)
  s <- qbinom(1-alpha/2, n, pstar)
  CI <- c(xs[r], xs[s])
  alpha1 <- pbinom(r-1, n, pstar)
  alpha2 <- 1-pbinom(s-1, n, pstar)
  level.act <- 1 - alpha1 - alpha2
  return(list(CI=CI, level.act=level.act))
}