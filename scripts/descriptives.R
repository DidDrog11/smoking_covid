##Run if needing to update the studies
source('scripts/data_cleaning.R')
##Run if data already available
library(tidyverse)
library(linelist)
source('scripts/prisma_function.R')

date_of_update <- Sys.Date()
date_of_last_update <- date_of_update-10
prev_versions <- c('v1', 'v2')
current_version <- c('v3')
analysed_versions <- c('v1', 'v2', 'v3')
exclude_from_analysis <- c('isaric_1', 'isaric_2', 'isaric_3', 'miyara_old')
exclude_from_qa <- c('ISARIC_1', 'ISARIC_2', 'ISARIC_3', 'Miyara')

search_details <- read_rds("data_clean/search_details.rds")

data_study_general <- read_rds("data_clean/data_study_general.rds")
data_study_general <- data_study_general %>%
  filter(review_version %in% analysed_versions) %>%
  filter(!lead_author %in% exclude_from_analysis) 

table_1 <- read_rds("data_clean/data_table_1.rds")
table_1 <- table_1 %>%
  filter(review_version %in% analysed_versions) %>%
  select(-review_version) %>%
  filter(!lead_author %in% exclude_from_analysis)

table_2 <- read_rds("data_clean/data_table_2.rds")
table_2 <- table_2 %>%
  filter(review_version %in% analysed_versions) %>%
  select(-review_version) %>%
  filter(!lead_author %in% exclude_from_analysis)

table_3 <- read_rds("data_clean/data_table_3.rds")
table_3 <- table_3 %>%
  filter(review_version %in% analysed_versions) %>%
  select(-review_version) %>%
  filter(!lead_author %in% exclude_from_analysis)

table_4 <- read_rds("data_clean/data_table_4.rds")
table_4 <- table_4 %>%
  filter(review_version %in% analysed_versions) %>%
  select(-review_version) %>%
  filter(!lead_author %in% exclude_from_analysis)

table_5 <- read_rds("data_clean/data_table_5.rds")
table_5 <- table_5 %>%
  filter(review_version %in% analysed_versions) %>%
  select(-review_version) %>%
  filter(!lead_author %in% exclude_from_analysis)

table_6 <- table_6 %>%
  filter(!Author %in% exclude_from_qa)
  

#Number screened to add to PRISMA
from_prev_version <- data_study_general %>%
  filter(review_version %in% prev_versions) %>%
  filter(!lead_author %in% exclude_from_analysis) %>%
  tally()


PRISMA_v3 <- PRISMA(search_details)
PRISMA_v3

#Countries
table(table_1$country)

#Setting
table(data_study_general$study_setting)

#Numbers
summary(table_1$sample_size)

#Number of participants
sum(table_1$sample_size)

#Source of data
table(table_1$data_source)

#Studies reporting smokers
current_smok <- table_1 %>%
  filter(., current_smoker != 'NA')
nrow(current_smok)

#Studies reporting former smokers
former_smok <- table_1 %>%
  filter(., former_smoker != 'NA')
nrow(former_smok)

#Studies reporting never smokers
never_smok <- table_1 %>%
  filter(., never_smoker != 'NA')
nrow(never_smok)

#Studies reporting current/former smokers
current_former_smok <- table_1 %>%
  filter(., current_former_smoker != 'NA')
nrow(current_former_smok)

#Studies reporting missing data
missing_smok <- table_1 %>%
  filter(., missing != 'NA')
nrow(missing_smok)

#Studies reporting never/unknown
never_smok_unknown <- table_1 %>%
  filter(., never_smoker_unknown != 'NA')
nrow(never_smok_unknown)

#Studies with not stated
smok_not_stated <- table_1 %>%
  filter(., not_stated != 'NA')
nrow(smok_not_stated)

#Studies reporting current, former and never smoking status
full_smoking_status <- table_1 %>%
  filter(lead_author %in% current_smok$lead_author) %>%
  filter(lead_author %in% former_smok$lead_author) %>%
  filter(lead_author %in% never_smok$lead_author)

#Studies reporting current or current/former and never smoking
semi_full_smoking_status <- table_1 %>%
  filter(lead_author %in% current_former_smok$lead_author) %>%
  filter(lead_author %in% never_smok$lead_author) %>%
  filter(!lead_author %in% full_smoking_status$lead_author)

#Remaining studies
incomplete_smoking_status <- table_1 %>%
  filter(!lead_author %in% full_smoking_status$lead_author) %>%
  filter(!lead_author %in% semi_full_smoking_status$lead_author)

#Smoking prevalence by country
country_prevalence <- table_1 %>%
  group_by(country) %>%
  mutate(., current_smok_percentage = current_smoker/total*100) %>%
  mutate(., former_smok_percentage = former_smoker/total*100) %>%
  mutate(., missing_percentage = missing/total*100)

china_prevalence <- country_prevalence %>%
  filter(country == 'china') %>%
  summary()

usa_prevalence <- country_prevalence %>%
  filter(country == 'usa') %>%
  summary()

uk_prevalence <- country_prevalence %>%
  filter(country == 'uk') %>%
  summary()

france_prevalence <- country_prevalence %>%
  filter(country == 'france') %>%
  summary()

mexico_prevalence <- country_prevalence %>%
  filter(country == 'mexico') %>%
  mutate(., not_stated_percentage = not_stated/total*100) %>%
  summary()

spain_prevalence <- country_prevalence %>%
  filter(country == 'spain') %>%
  summary()

italy_prevalence <- country_prevalence %>%
  filter(country == 'italy') %>%
  summary()

iran_prevalence <- country_prevalence %>%
  filter(country == 'iran') %>%
  summary()

israel_prevalence <- country_prevalence %>%
  filter(country == 'israel') %>%
  mutate(never_smoker_percentage = never_smoker/total*100) %>%
  summary()

korea_prevalence <- country_prevalence %>%
  filter(country == 'korea') %>%
  summary()

kuwait_prevalence <- country_prevalence %>%
  filter(country == 'kuwait') %>%
  summary()

switzerland_prevalence <- country_prevalence %>%
  filter(country == 'switzerland') %>%
  summary()

#Updating table 1
table_1_word <- table_1 %>%
  mutate(., current_percentage = current_smoker/total*100) %>%
  mutate(., former_percentage = former_smoker/total*100) %>%
  mutate(., current_former_percentage = current_former_smoker/total*100) %>%
  mutate(., never_smoker_percentage = never_smoker/total*100) %>%
  mutate(., never_smoker_unknown_percentage = never_smoker_unknown/total*100) %>%
  mutate(., not_stated_percentage = not_stated/total*100) %>%
  mutate(., missing_percentage = missing/total*100) %>%
  select(lead_author, date_published, country, sample_size, median_age, iqr_lower, iqr_upper, mean_age, lower_range, upper_range, standard_deviation, female_sex_percent,
         current_percentage, former_percentage, current_former_percentage, never_smoker_percentage, never_smoker_unknown_percentage,
         not_stated_percentage, missing_percentage)

a <- data_study_general %>%
  select(lead_author, study_setting)

table_1_word <- left_join(table_1_word, a, by = 'lead_author') %>%
  select(1:4, 20, 5:19)

write_rds(table_1_word, 'data_clean/table_1_word.rds')

#Table 2
table_2_word <-  table_2 %>%
  mutate(., sample = contributing_sample) %>%
  mutate(., negative_test_percentage = negative_test/sample*100,
         negative_current_percentage = negative_current_smoker/negative_test*100,
         negative_former_smoker_percentage = negative_former_smoker/negative_test*100,
         negative_current_former_smoker_percentage = negative_current_former_smoker/negative_test*100,
         negative_never_smoker_percentage = negative_never_smoker/negative_test*100,
         negative_not_stated_percentage = negative_not_stated/negative_test*100, 
         positive_test_percentage = positive_test/sample*100, 
         positive_current_smoker_percentage = positive_current_smoker/positive_test*100,
         positive_former_smoker_percentage = positive_former_smoker/positive_test*100,
         positive_current_former_smoker_percentage = positive_current_former_smoker/positive_test*100, 
         positive_never_smoker_percentage = positive_never_smoker/positive_test*100, 
         positive_not_stated_percentage = positive_not_stated/positive_test*100) %>%
  select(-data_on_testing, -missing, -date_screened, -sample)

write_rds(table_2_word, 'data_clean/table_2_word.rds')

quality_table_2 <- table_2_word %>%
  left_join(., quality_rating, by = 'lead_author') %>%
  select(lead_author, overall_rating, )

#Table 3
table_3_word <- table_3 %>%
  mutate(., sample = sample_with_outcome) %>%
  mutate(., community_percentage = number_community/sample*100) %>%
  mutate(., community_current_smoker_percent = community_current_smoker/number_community*100) %>%
  mutate(., community_former_smoker_percent = community_former_smoker/number_community*100) %>%
  mutate(., community_current_former_smoker_percent = community_current_former_smoker/number_community*100) %>%
  mutate(., community_never_smoker_percent = community_never_smoker/number_community*100) %>%
  mutate(., community_never_unknown_smoker_percent = community_never_unknown_smoker/number_community*100) %>%
  mutate(., community_not_stated_percent = community_not_stated/number_community*100) %>%
  mutate(., number_hospitalised_percent = number_hospitalised/sample*100) %>%
  mutate(., hospitalised_current_smoker_percent = hospitalised_current_smoker/number_hospitalised*100) %>%
  mutate(., hospitalised_former_smoker_percent = hospitalised_former_smoker/number_hospitalised*100) %>%
  mutate(., hospitalised_current_former_smoker_percent = hospitalised_current_former_smoker/number_hospitalised*100) %>%
  mutate(., hospitalised_never_smoker_percent = hospitalised_never_smoker/number_hospitalised*100) %>%
  mutate(., hospitalised_never_unknown_smoker_percent = hospitalised_never_unknown_smoker/number_hospitalised*100) %>%
  mutate(., hospitalised_not_stated_percent = hospitalised_not_stated/number_hospitalised*100) %>%
  select(1, 20, 6, 21, 7, 22, 8, 23, 9, 24, 10, 25, 11, 26, 12, 27, 13, 28, 14, 29, 15, 30, 16, 31, 17, 32, 18, 33, 19, 34)

write_rds(table_3_word, 'data_clean/table_3_word.rds')

#Table 4
table_4_word <- table_4 %>%
  mutate(., sample = sample_with_severity) %>%
  mutate(., non_severe_disease_percentage = non_severe_disease/sample*100) %>%
  mutate(., non_severe_current_smoker_percent = non_severe_current_smoker/non_severe_disease*100) %>%
  mutate(., non_severe_former_smoker_percent = non_severe_former_smoker/non_severe_disease*100) %>%
  mutate(., non_severe_current_former_smoker_percent = non_severe_current_former_smoker/non_severe_disease*100) %>%
  mutate(., non_severe_never_smoker_percent = non_severe_never_smoker/non_severe_disease*100) %>%
  mutate(., non_severe_never_unknown_smoker_percent = non_severe_never_unknown_smoker/non_severe_disease*100) %>%
  mutate(., non_severe_not_stated_percent = non_severe_not_stated/non_severe_disease*100) %>%
  mutate(., severe_disease_number_percent = severe_disease_number/sample*100) %>%
  mutate(., severe_disease_current_smoker_percent = severe_disease_current_smoker/severe_disease_number*100) %>%
  mutate(., severe_disease_former_smoker_percent = severe_disease_former_smoker/severe_disease_number*100) %>%
  mutate(., severe_disease_current_former_smoker_percent = severe_disease_current_former_smoker/severe_disease_number*100) %>%
  mutate(., severe_disease_never_smoker_percent = severe_disease_never_smoker/severe_disease_number*100) %>%
  mutate(., severe_disease_never_unknown_percent = severe_disease_never_unknown/severe_disease_number*100) %>%
  mutate(., severe_disease_not_stated_percent = severe_disease_not_stated/severe_disease_number*100) %>%
  select(1, 19, 5, 20, 6, 21, 7, 22, 8, 23, 9, 24, 10, 25, 11, 26, 12, 27, 13, 28, 14, 29, 15, 30, 16, 31, 17, 32, 18, 33)

write_rds(table_4_word, 'data_clean/table_4_word.rds')

#Table 5
table_5_word <- table_5 %>%
  mutate(., sample = sample_with_deaths) %>%
  mutate(., deaths_percentage = deaths/sample*100) %>%
  mutate(., death_current_smokers_percent = death_current_smokers/deaths*100) %>%
  mutate(., death_former_smokers_percent = death_former_smokers/deaths*100) %>%
  mutate(., death_current_former_smokers_percent = death_current_former_smokers/deaths*100) %>%
  mutate(., death_never_smokers_percent = death_never_smokers/deaths*100) %>%
  mutate(., death_never_unknown_smokers_percent = death_never_unknown_smokers/deaths*100) %>%
  mutate(., death_not_stated_percent = death_not_stated/deaths*100) %>%
  mutate(., recovered_percentage = recovered/sample*100) %>%
  mutate(., recovered_current_smoking_percent = recovered_current_smoking/recovered*100) %>%
  mutate(., recovered_former_smoker_percent = recovered_former_smoker/recovered*100) %>%
  mutate(., recovered_current_former_smokers_percent = recovered_current_former_smokers/recovered*100) %>%
  mutate(., recovered_never_smoker_percent = recovered_never_smoker/recovered*100) %>%
  mutate(., recovered_never_unknown_smoker_percent = recovered_never_unknown_smoker/recovered*100) %>%
  mutate(., recovered_not_stated_percent = recovered_not_stated/recovered*100) %>%
  select(1, 20, 6, 21, 7, 22, 8, 23, 9, 24, 10, 25, 11, 26, 12, 27, 13, 28, 14, 29, 15, 30, 16, 31, 17, 32, 18, 33, 19, 34)

write_rds(table_5_word, 'data_clean/table_5_word.rds')

#Table 6
table_6_word <- table_6

quality_rating <- table_6_word %>%
  select(Author, 16) %>%
  rename('lead_author' = Author, 'overall_rating' = 2) %>%
  clean_data()

write_rds(table_6_word, 'data_clean/table_6_word.rds')
