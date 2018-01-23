sqranks.test.approx <- function(x, y, alternative=c("two.sided", "greater", "less"))
{
  ## check for partial matches of the alternative argument
  alternative <- match.arg(alternative)
  
  ## calculate the absolute discrepancies from the (estimated) mean
  xbar <- mean(x)
  ybar <- mean(y)
  u <- abs(x - xbar)
  v <- abs(y - ybar)
  
  ## calculate the observed ranks, and t
  r <- rank(c(u, v))
  ru <- r[seq_along(u)]
  rv <- r[-seq_along(u)]
  t <- sum(ru^2)
  
  ## calculate the standardized version, t1
  nx <- length(x)
  ny <- length(y)
  N <- nx + ny
  r2bar <- (sum(ru^2) + sum(rv^2))/N
  r4sum <- sum(r^4)
  t1 <- (t - nx*r2bar)/sqrt(nx*ny/(N*(N-1))*r4sum - nx*ny/(N-1)*r2bar^2)
  
  ## calculate the p-value
  if(alternative == "two.sided") 
    phi <- 2*pnorm(-abs(t1))
  else if(alternative == "less")
    phi <- pnorm(t1)
  else if(alternative == "greater")
    phi <- pnorm(t1, lower.tail=FALSE)
  
  ## return the stats and p-value
  return(list(t=t, t1=t1, p.value=phi))
}