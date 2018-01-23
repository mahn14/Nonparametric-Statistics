# Quantile Test

#approximates two-tail test

quantile.test<-function(x,xstar=0,quantile=.5,alternative="two.sided"){
  n<-length(x)
  p<-quantile
  T1<-sum(x<=xstar)
  T2<-sum(x< xstar)
  if (alternative=="quantile.less") {
    p.value<-1-pbinom(T2-1,n,p)}
  if (alternative=="quantile.greater"){
    p.value<-pbinom(T1,n,p)}
  if (alternative=="two.sided"){
    if (T1 == T2) p.value <- binom.test(T1, n, p)$p.value #added by gramacy
    p.value<-2*min(1-pbinom(T2-1,n,p),pbinom(T1,n,p))}
  list(xstar=xstar,alternative=alternative,T1=T1,T2=T2,p.value=p.value)}