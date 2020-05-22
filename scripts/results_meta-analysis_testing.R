library(epitools)
library(meta)
library(tidyverse)
source('scripts/rr_function.R')

table_2 <- read_rds('data_clean/table_2_word.rds')
included_studies <- c('cho', 'de_lusignan', 'rentsch', 'kolin', 'shah')

#And Nidzweidz

#################RR from raw data#####################

meta <- tibble('author' = table_2$lead_author,
               'negative_smoker' = table_2$negative_current_smoker,
               'negative_never_smoker' = table_2$negative_never_smoker,
               'positive_smoker' = table_2$positive_current_smoker,
               'positive_never_smoker' = table_2$positive_never_smoker,
               'negative_former_smoker' = table_2$negative_former_smoker,
               'positive_former_smoker' = table_2$positive_former_smoker) %>%
        filter(author %in% included_studies)

meta$author <- recode(meta$author, 'rentsch' = 'Rentsch',
                      'cho' = 'Cho',
                      'shah' = 'Shah',
                      'kolin' = 'Kolin',
                      'de_lusignan' = 'de Lusignan')

a <- RR_testing('Rentsch', 'current')
b <- RR_testing('Rentsch', 'former')
c <- RR_testing('Cho', 'current')
d <- RR_testing('Cho', 'former')
e <- RR_testing('Shah', 'current')
f <- RR_testing('Shah', 'former')
g <- RR_testing('Kolin', 'current')
h <- RR_testing('Kolin', 'former')
i <- RR_testing('de Lusignan', 'current')
j <- RR_testing('de Lusignan', 'former')
data <- list(a,b,c,d,e,f,g,h,i,j)
k <- do.call(rbind.data.frame, data)
# Rentsch -----------------------------------------------------------------
#current vs. never smokers

negative_test_smoker <- 1444
positive_test_smoker <- 159
negative_test_never_smoker <- 826
positive_test_never_smoker <- 216

row_1 <- c(negative_test_never_smoker, positive_test_never_smoker)
row_2 <- c(negative_test_smoker, positive_test_smoker)

rentsch_table_1 <- rbind(row_1, row_2)
rownames(rentsch_table_1) <- c('never smokers', 'current smokers')
colnames(rentsch_table_1) <- c('negative test', 'positive test')

rentsch_1 <- riskratio(x = rentsch_table_1, method = 'wald', conf.level=0.95)
lower_ci <- log(rentsch_1[["measure"]][2,2])
upper_ci <- log(rentsch_1[["measure"]][2,3])

rentsch_log_RR_1 <- log(rentsch_1[["measure"]][2,1])
rentsch_log_SE_1 <-(upper_ci-lower_ci)/3.92

#former vs. never smokers

negative_test_former_smoker <- 704
positive_test_former_smoker <- 179
negative_test_never_smoker <- 826
positive_test_never_smoker <- 216

row_1 <- c(negative_test_never_smoker, positive_test_never_smoker)
row_2 <- c(negative_test_former_smoker, positive_test_former_smoker)

rentsch_table_2 <- rbind(row_1, row_2)
rownames(rentsch_table_2) <- c('never smokers', 'former smokers')
colnames(rentsch_table_2) <- c('negative test', 'positive test')

rentsch_2 <- riskratio(x = rentsch_table_2, method = 'wald', conf.level=0.95)
lower_ci <- log(rentsch_2[["measure"]][2,2])
upper_ci <- log(rentsch_2[["measure"]][2,3])

rentsch_log_RR_2 <- log(rentsch_2[["measure"]][2,1])
rentsch_log_SE_2 <-(upper_ci-lower_ci)/3.92



# Fontanet ----------------------------------------------------------------
#Fontanet et al. 2020

negative_test_smoker <- 64
positive_test_smoker <- 5
negative_test_never_smoker <- 426
positive_test_never_smoker <- 166

row_1 <- c(negative_test_never_smoker, positive_test_never_smoker)
row_2 <- c(negative_test_smoker, positive_test_smoker)

fontanet_table_1 <- rbind(row_1, row_2)
rownames(fontanet_table_1) <- c('never smokers', 'current smokers')
colnames(fontanet_table_1) <- c('negative test', 'positive test')

fontanet_1 <- riskratio(x = fontanet_table_1, method = 'wald', conf.level=0.95)


# Niedzwiedz --------------------------------------------------------------
#SEs for Niedzwiedz et al. 2020

#current vs. never

niedz_log_RR_1<-log(1.15)
niedz_log_SE_1<-(log(1.54)-log(0.86))/3.92


#former vs. never

niedz_log_RR_2<-log(1.42)
niedz_log_SE_2<-(log(1.69)-log(1.19))/3.92

# Cho ---------------------------------------------------------------------
#Cho et al. 2020

#current vs. never
negative_test_smoker <- 142
negative_test_never_smoker <- 437
positive_test_smoker <- 111
positive_test_never_smoker <- 282

row_1 <- c(negative_test_never_smoker, positive_test_never_smoker)
row_2 <- c(negative_test_smoker, positive_test_smoker)

cho_table_1 <- rbind(row_1, row_2)
rownames(cho_table_1) <- c('never smokers', 'current smokers')
colnames(cho_table_1) <- c('negative test', 'positive test')

cho_1 <- riskratio(x = cho_table_1, method = 'wald', conf.level=0.95)
lower_ci <- log(cho_1[["measure"]][2,2])
upper_ci <- log(cho_1[["measure"]][2,3])

cho_log_RR_1 <- log(cho_1[["measure"]][2,1])
cho_log_SE_1 <-(upper_ci-lower_ci)/3.92

#former vs. never smokers

negative_test_former_smoker <- 214
positive_test_former_smoker <- 145


row_1 <- c(negative_test_never_smoker, positive_test_never_smoker)
row_2 <- c(negative_test_former_smoker, positive_test_former_smoker)

cho_table_2 <- rbind(row_1, row_2)
rownames(cho_table_2) <- c('never smokers', 'former smokers')
colnames(cho_table_2) <- c('negative test', 'positive test')

cho_2 <- riskratio(x = cho_table_2, method = 'wald', conf.level=0.95)
lower_ci <- log(cho_2[["measure"]][2,2])
upper_ci <- log(cho_2[["measure"]][2,3])

cho_log_RR_2 <- log(cho_2[["measure"]][2,1])
cho_log_SE_2 <-(upper_ci-lower_ci)/3.92


# Shah --------------------------------------------------------------------

#current vs. never
negative_test_smoker <- 52
negative_test_never_smoker <- 113
positive_test_smoker <- 0
positive_test_never_smoker <- 20

row_1 <- c(negative_test_never_smoker, positive_test_never_smoker)
row_2 <- c(negative_test_smoker, positive_test_smoker)

shah_table_1 <- rbind(row_1, row_2)
rownames(shah_table_1) <- c('never smokers', 'current smokers')
colnames(shah_table_1) <- c('negative test', 'positive test')

shah_1 <- riskratio(x = shah_table_1, method = 'wald', conf.level=0.95)
lower_ci <- log(shah_1[["measure"]][2,2])
upper_ci <- log(shah_1[["measure"]][2,3])

shah_log_RR_1 <- log(shah_1[["measure"]][2,1])
shah_log_SE_1 <-(upper_ci-lower_ci)/3.92

#former vs. never smokers

negative_test_former_smoker <- 47
positive_test_former_smoker <- 9


row_1 <- c(negative_test_never_smoker, positive_test_never_smoker)
row_2 <- c(negative_test_former_smoker, positive_test_former_smoker)

shah_table_2 <- rbind(row_1, row_2)
rownames(shah_table_2) <- c('never smokers', 'former smokers')
colnames(shah_table_2) <- c('negative test', 'positive test')

shah_2 <- riskratio(x = shah_table_2, method = 'wald', conf.level=0.95)
lower_ci <- log(shah_2[["measure"]][2,2])
upper_ci <- log(shah_2[["measure"]][2,3])

shah_log_RR_2 <- log(shah_2[["measure"]][2,1])
shah_log_SE_2 <-(upper_ci-lower_ci)/3.92


# Bello-Chavolla ----------------------------------------------------------

#current/former vs. not stated

negative_test_current_former <- 4835
negative_test_not_stated <- 42125
positive_test_current_former <- 1374
positive_test_not_stated <- 14155

row_1 <- c(negative_test_not_stated, positive_test_not_stated)
row_2 <- c(negative_test_current_former, positive_test_current_former)

bello_table_1 <- rbind(row_1, row_2)
rownames(bello_table_1) <- c('not stated', 'current/former smokers')
colnames(bello_table_1) <- c('negative test', 'positive test')

bello_1 <- riskratio(x = bello_table_1, method = 'wald', conf.level=0.95)


# Kolin ------------------------------------------------------------------

#current vs. never
negative_test_smoker <- 141
negative_test_never_smoker <- 354
positive_test_smoker <- 72
positive_test_never_smoker <- 303

row_1 <- c(negative_test_never_smoker, positive_test_never_smoker)
row_2 <- c(negative_test_smoker, positive_test_smoker)

kolin_table_1 <- rbind(row_1, row_2)
rownames(kolin_table_1) <- c('never smokers', 'current smokers')
colnames(kolin_table_1) <- c('negative test', 'positive test')

kolin_1 <- riskratio(x = kolin_table_1, method = 'wald', conf.level=0.95)
lower_ci <- log(kolin_1[["measure"]][2,2])
upper_ci <- log(kolin_1[["measure"]][2,3])

kolin_log_RR_1 <- log(kolin_1[["measure"]][2,1])
kolin_log_SE_1 <-(upper_ci-lower_ci)/3.92

#former vs. never smokers

negative_test_former_smoker <- 307
positive_test_former_smoker <- 285


row_1 <- c(negative_test_never_smoker, positive_test_never_smoker)
row_2 <- c(negative_test_former_smoker, positive_test_former_smoker)

kolin_table_2 <- rbind(row_1, row_2)
rownames(kolin_table_2) <- c('never smokers', 'former smokers')
colnames(kolin_table_2) <- c('negative test', 'positive test')

kolin_2 <- riskratio(x = kolin_table_2, method = 'wald', conf.level=0.95)
lower_ci <- log(kolin_2[["measure"]][2,2])
upper_ci <- log(kolin_2[["measure"]][2,3])

kolin_log_RR_2 <- log(kolin_2[["measure"]][2,1])
kolin_log_SE_2 <-(upper_ci-lower_ci)/3.92



# de Lusignan -------------------------------------------------------------
negative_smoker <- 366
positive_smoker <- 47
negative_never_smoker <- 924
positive_never_smoker <- 201

row_1 <- c(negative_never_smoker, positive_never_smoker)
row_2 <- c(negative_smoker, positive_smoker)

de_lusignan_table_1 <- rbind(row_1, row_2)
rownames(de_lusignan_table_1) <- c('never smokers', 'current smokers')
colnames(de_lusignan_table_1) <- c('negative test', 'positive test')

de_lusignan_table_1 <- riskratio(x = de_lusignan_table_1, method = 'wald', conf.level=0.95)
lower_ci <- log(de_lusignan_table_1[["measure"]][2,2])
upper_ci <- log(de_lusignan_table_1[["measure"]][2,3])

de_lusignan_log_RR_1 <- log(de_lusignan_table_1[["measure"]][2,1])
de_lusignan_log_SE_1 <-(upper_ci-lower_ci)/3.92

#former vs. never smokers
negative_former_smoker <- 1450
positive_former_smoker <- 303
negative_never_smoker <- 924
positive_never_smoker <- 201

row_1 <- c(negative_never_smoker, positive_never_smoker)
row_2 <- c(negative_former_smoker, positive_former_smoker)

de_lusignan_table_2 <- rbind(row_1, row_2)
rownames(de_lusignan_table_2) <- c('never smokers', 'former smokers')
colnames(de_lusignan_table_2) <- c('negative test', 'positive test')

de_lusignan_table_2 <- riskratio(x = de_lusignan_table_2, method = 'wald', conf.level=0.95)
lower_ci <- log(de_lusignan_table_2[["measure"]][2,2])
upper_ci <- log(de_lusignan_table_2[["measure"]][2,3])

de_lusignan_log_RR_2 <- log(de_lusignan_table_2[["measure"]][2,1])
de_lusignan_log_SE_2 <-(upper_ci-lower_ci)/3.92

# For poor quality studies text ----------------------------------------------------------------
RR <- fontanet_1$measure[[2,1]]
Lower_CI <- fontanet_1$measure[[2,2]]
Upper_CI <- fontanet_1$measure[[2,3]]
p_value_Chi <- fontanet_1$p.value[[2,3]]
Fontanet_results <- cbind(RR, Lower_CI, Upper_CI, p_value_Chi)
rownames(Fontanet_results) <- 'Fontanet et al. 2020'
Fontanet_results

RR <- bello_1$measure[[2,1]]
Lower_CI <- bello_1$measure[[2,2]]
Upper_CI <- bello_1$measure[[2,3]]
p_value_Chi <- bello_1$p.value[[2,3]]
bello_results <- cbind(RR, Lower_CI, Upper_CI, p_value_Chi)
rownames(bello_results) <- 'Bello et al. 2020'
bello_results

# Meta-Analysis -----------------------------------------------------------

# Testing -----------------------------------------------------------------
numbers_in_analysis <- table_2_word %>%
        left_join(., quality_rating, by = 'lead_author') %>%
        filter(., overall_rating != 'poor') %>%
        add_row(lead_author = 'niedzwiedz', contributing_sample = 1474) %>%
        replace_na(list(negative_not_stated = 0, positive_not_stated = 0)) %>%
        mutate(contributing_sample = (contributing_sample - (negative_not_stated+positive_not_stated))) %>%
                       select(lead_author, contributing_sample)
included_studies <- c('Rentsch, n = 3528', 'Cho, n = 1331', 'Shah, n = 243', 'Kolin, n = 1462', 'Niedzwiedz, n = 1474', 'de Lusignan, n = 3291')


#current vs. never smokers

TE<-c(rentsch_log_RR_1,cho_log_RR_1,shah_log_RR_1, kolin_log_RR_1,niedz_log_RR_1, de_lusignan_log_RR_1)
seTE<-c(rentsch_log_SE_1,cho_log_SE_1, shah_log_SE_1 ,kolin_log_SE_1,niedz_log_SE_1, de_lusignan_log_SE_1)
a<-metagen(TE,seTE,sm="RR", studlab = included_studies, comb.fixed = F, comb.random = T)

png("figure/fig_2.png", width=1024, height=546, res=120)
forest(a,
       sortvar = included_studies,
       xlim = c(0.3, 2),
       rightlabs = c('RR', '95% CI', 'Weight'),
       leftlabs = c('Author', 'logRR', 'SE'),
       print.tau2 = F,
       col.diamond = 'blue',
       col.diamond.lines = 'black',
       col.square = 'black',
       col.square.lines = 'black',
       digits.sd = 2,
       colgap.forest.left = unit(15,"mm"))
dev.off()

#former vs. never smokers

TE<-c(rentsch_log_RR_2,cho_log_RR_2,shah_log_RR_2,kolin_log_RR_2,niedz_log_RR_2, de_lusignan_log_RR_2)
seTE<-c(rentsch_log_SE_2,cho_log_SE_2,shah_log_SE_2,kolin_log_SE_2,niedz_log_SE_2, de_lusignan_log_SE_2)
b<-metagen(TE,seTE,sm="RR", studlab = included_studies, comb.fixed = F, comb.random = T)

png("figure/fig_3.png", width=1024, height=546, res=120)
forest(b,
       sortvar = included_studies,
       xlim = c(0.5, 3),
       rightlabs = c('RR', '95% CI', 'Weight'),
       leftlabs = c('Author', 'logRR', 'SE'),
       print.tau2 = F,
       col.diamond = 'blue',
       col.diamond.lines = 'black',
       col.square = 'black',
       col.square.lines = 'black',
       digits.sd = 2)
dev.off()
