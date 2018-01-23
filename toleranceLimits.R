



# translates from P( x^(r) <= at least q proportion of pop <= x^(r+m-1) ) <= alpha
bintol <- function(n, r, m, q)
{
  i <- 0:(r+m-1)
  sum(choose(n, i) * (1-q)^i * q^(n-i))
}

# Solves summation exactly
solve.bintol.n <- function(r, m, q, alpha)
{
  n <-1
  repeat {
    p <- bintol(n, r, m, q)
    if(p < alpha) break
    else n <- n+1
  }
}

# Solves approximation by algebra
approx.bintol.n <- function(r, m, q, alpha)
{
  x <- qchisq(1-alpha, 2*(r+m))
  (1/4)*x*(1+q)/(1-q) + (1/2)*(r+m-1)
}