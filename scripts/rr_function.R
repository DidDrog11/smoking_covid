RR_testing <- function(study, smoking_RR){
  if(smoking_RR == 'current'){
  a <- filter(meta, author == study)
  b <- a %>%
    select(negative_never_smoker, positive_never_smoker) %>%
    rename('negative test' = negative_never_smoker, 'positive test' = positive_never_smoker)
  c <- a %>%
    select(negative_smoker, positive_smoker) %>%
    rename('negative test' = negative_smoker, 'positive test' = positive_smoker)
  a <- as.matrix(rbind(b, c))
  rownames(a) <- c('never smokers', 'current smokers')
  } else {
  a <- filter(meta, author == study)
  b <- a %>%
    select(negative_never_smoker, positive_never_smoker) %>%
    rename('negative test' = negative_never_smoker, 'positive test' = positive_never_smoker)
  c <- a %>%
    select(negative_former_smoker, positive_former_smoker) %>%
    rename('negative test' = negative_former_smoker, 'positive test' = positive_former_smoker)
  a <- as.matrix(rbind(b, c))
  rownames(a) <- c('never smokers', 'former smokers')
  }
  
  a <- riskratio(a, method = 'wald', conf.level=0.95)
  
  log_RR <- log(a[["measure"]][2,1])
  lower_ci <- log(a[["measure"]][2,2])
  upper_ci <- log(a[["measure"]][2,3])
  log_SE <- (upper_ci-lower_ci)/3.92

  output_table <- list('study' = study,
                       'smoking_status' = smoking_RR,
                       'log_RR' = log_RR,
                       'lower_ci' = lower_ci, 
                       'upper_ci' = upper_ci,
                       'log_SE' = log_SE)
  return(as_tibble(output_table))
}
