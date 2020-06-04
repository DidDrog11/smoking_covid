boot.mean = function(x,B,binwidth=NULL) {
  n = length(x)
  boot.samples = matrix(sample(x,size=n*B,replace=TRUE), B, n)
  boot.statistics = apply(boot.samples,1,mean)
  se = sd(boot.statistics)
  interval=quantile(boot.statistics,c(0.025,0.975))
  return(list(boot.statistics=boot.statistics,interval=interval,se=se))
}