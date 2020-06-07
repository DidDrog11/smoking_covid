boot.mean = function(x,B,binwidth=NULL) {
  n = length(x)
  boot.samples = matrix(sample(x,size=n*B,replace=TRUE), B, n)
  boot.statistics = apply(boot.samples,1,mean)
  se = sd(boot.statistics)
  interval=quantile(boot.statistics,c(0.025,0.975))
  return(list(boot.statistics=boot.statistics,interval=interval,se=se))
}

do_bootstrap_current <- function (row_number) {
  my_sample <- c(rep(1, each = bootstrap$bs_current[row_number]), rep(0, each = bootstrap$bs_inverse[row_number]))
  bootstrap_output_c[[row_number]] <- boot.mean(my_sample,1000)$interval
  return(bootstrap_output_c)
}

do_bootstrap_former <- function (row_number) {
  my_sample <- c(rep(1, each = bootstrap$bs_former[row_number]), rep(0, each = bootstrap$bs_inverse_former[row_number]))
  bootstrap_output_f[[row_number]] <- boot.mean(my_sample,1000)$interval
  return(bootstrap_output_f)
}
