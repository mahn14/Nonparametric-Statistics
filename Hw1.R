    # Problem 1: Sample Space ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#on document


    # Problem 2: Probability ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#a.) on document
#b.)
p = 0.2
x = 3
n = 4
ans = (factorial(n) / (factorial(x)*factorial(n-x))) * p^3 * (1-p)
ans     


    # Problem 3: Binomial Distribution ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# P(recovery) = 0.4, clients = 15
p = 0.4
n = 15

#a.) P(at least 10 survive)
ans = 0
for (k in c(10:15)) {
  ans <- ans + (factorial(n) / (factorial(k)*factorial(n-k)))* p^k * (1-p)^(n-k)
}
ans

#b.) P(3 to 8 survive)
ans = 0
for (k in c(3:8)) {
  ans <- ans + (factorial(n) / (factorial(k)*factorial(n-k)))* p^k * (1-p)^(n-k)
}
ans

#c.) P(exactly 5 survive)
k = 5
ans = 0
ans <- (factorial(n) / (factorial(k)*factorial(n-k)))* p^k * (1-p)^(n-k)
ans


  # Problem 4: Binomial Probabilities ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#a.) P(guesswork on 80 will yield < 20 correct)
n = 80
p = 0.25
ans = 0
for (k in c(1:19)) {
  ans <- ans + (factorial(n) / (factorial(k)*factorial(n-k)))* p^k * (1-p)^(n-k)
}
ans

#b.) P(guesswork on 80 will yield more than 40 correct answers)
n = 80
p = 0.25
ans = 0
for (k in c(41:80)) {
  ans <- ans + (factorial(n) / (factorial(k)*factorial(n-k)))* p^k * (1-p)^(n-k)
}
ans

#c.) P(25 to 30 correct answers, inclusive)
n = 80
p = 0.25
ans = 0
for (k in c(25:30)) {
  ans <- ans + (factorial(n) / (factorial(k)*factorial(n-k)))* p^k * (1-p)^(n-k)
}
ans


  # Problem 5: Confidence Intervals ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
times <- c(103, 94, 110, 87, 98, 97, 82, 123, 92, 175, 88, 118)
#assume normal distribution

#a.) Find 95% CI for mean
xbar <- mean(times)
n = length(times)
qt95 <- qt(0.975, df = n-1)
times.se = sd(times)/sqrt(n)
CI <- c(xbar - qt95*times.se, xbar + qt95*times.se)
CI

#b.) Find 95% CI for variance
n = length(times)
var = sd(times)
q1 <- qchisq(0.025, df = n - 1)
q2 <- qchisq(0.975, df = n - 1)
CI <- c((n-1)*var / q2, (n-1)*var / q1)
CI

#c.) Find 90% for difference, equal variance
c1 <- c(103, 94, 110, 87,  98)
c2 <- c(97, 82, 123, 92, 175,  88, 118)
n1 <- length(c1)
n2 <- length(c2)
s1 <- sd(c1)
s2 <- sd(c2)
x1bar <- mean(c1)
x2bar <- mean(c2)
s.poolsquare <- ((n1-1)*s1^2 + (n2-1)*s2^2) / (n1+n2-2)
t95 <- qt(0.95, df = n1+n2-2)

CI <- c( (x1bar - x2bar) - t95*sqrt(s.poolsquare/n1 + s.poolsquare/n2), 
         (x1bar - x2bar) + t95*sqrt(s.poolsquare/n1 + s.poolsquare/n2))
CI

#d.) Find 90% CI for difference, unequal variance
se <- s1^2/n1 + s2^2/n2
v <- se^2 / ( s1^4/(n1^2*(n1-1)) + s2^4 / (n2^2*(n2-1)))
t95 <- qt(0.95, v)

CI <- c( (x1bar - x2bar) - t95*sqrt(se), (x1bar - x2bar) + t95*sqrt(se) )
CI


    # Problem 6: CI for Proportions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#a.) 95% CI for sample proportion
n <- 500
x <- 345
sp <- 345/500
se <- sqrt( (sp*(1- sp))/ n )
z95 <- qt(0.975, df = n-1)

CI <- c(sp - z95*se, sp + z95*se)
CI

#b.) 95% CI for difference in proportion
n1 <- 1500
n2 <- 2000
p1 <- 75/n1
p2 <- 80/n2
q90 <- qnorm(0.95)
sigmap <- sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)

CI <- c( (p1-p2) - q90*sigmap, (p1-p2) + q90*sigmap )
CI


    # Problem 7: Hypothesis Test ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#a.) Chance of u = 46 given sample, alpha = 0.05
u <- 46
n <- 12
xbar <- 42
sd <- 11.9
tstat <- (xbar - u) / (sd / sqrt(n))
phi <- 2*pt(-abs(tstat), n-1)
phi

#b.) Significant difference in means >= 2, alpha = 0.05
n1 <- 12
n2 <- 10
x1bar <- 85
x2bar <- 81
s1 <- 4
s2 <- 5

se <- sqrt(s1^2/n1 + s2^2/n2)
df <- se^2 / ((s1^2/n1)^2 / (n1-1) + (s2^2/n2)^2 / (n2-1))
tstat <- ((x1bar - x2bar) - 2) / se

phi <- pt(-abs(tstat), df)
phi

#c.) Significant test using sample, alpha = 0.01
pstar <- 0.7
t <- 8
n <- 15
rhs <- seq(ceiling(n*pstar), n) 
tU <- sum(dbinom(rhs, n, pstar) <= dbinom(t, n, pstar))
phi <- pbinom(t, n, pstar) + pbinom(n - tU, n, pstar, lower.tail=FALSE)
phi

#d.)
n1 <- 200
n2 <- 500
p1 <- 120/200
p2 <- 240/500

p <- (p1*n1 +p2*n2)/ (n1+n2)
se <- sqrt( p*(1-p) * ((1/n1) + (1/n2)) )
z <- (p1 - p2) / se

pt(-abs(z), 698)



