library(fmsb)
library(epitools)
library(meta)
library(tidyverse)

table_5 <- read_csv('data_clean/table_5_word.csv')
included_studies <- c('gaibazzi')

alive_smoker <- 11
dead_smoker <- 10
alive_never_smoker <- 244
dead_never_smoker <- 132
alive_former_smoker <- 30
dead_former_smoker <- 14

row_1 <- c(alive_never_smoker, dead_never_smoker)
row_2 <- c(alive_smoker, dead_smoker)

gaibazzi_table_1 <- rbind(row_1, row_2)
rownames(gaibazzi_table_1) <- c('never smokers', 'current smokers')
colnames(gaibazzi_table_1) <- c('alive', 'dead')

gaibazzi_1 <- riskratio(x = gaibazzi_table_1, method = 'wald', conf.level=0.95)

row_1 <- c(alive_never_smoker, dead_never_smoker)
row_2 <- c(alive_former_smoker, dead_former_smoker)

gaibazzi_table_2 <- rbind(row_1, row_2)
rownames(gaibazzi_table_2) <- c('never smokers', 'former smokers')
colnames(gaibazzi_table_2) <- c('alive', 'dead')

gaibazzi_2 <- riskratio(x = gaibazzi_table_2, method = 'wald', conf.level=0.95)


table_1 <- table_1 %>%
  
  mutate(., missing_1 = not_stated+missing/total)
