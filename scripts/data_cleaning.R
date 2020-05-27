library('tidyverse')
library('dplyr')
library('readxl')
library('linelist')
library('googlesheets4')

sheets_id <- as_sheets_id('https://docs.google.com/spreadsheets/d/15avypGR8ypJngWQEmFIzFrOOwXPY3xezUHQU6jgV7d0/edit?usp=sharing')
sheets_id

search_details <- read_sheet(sheets_id, range = 'article_screening')
  

search_details <- search_details %>%
  clean_data() %>%
  rename(date_screened = date_screening_in_format_2020_05_22) %>%
  write_rds(here::here('data_clean', 'search_details.rds')

data_study_general <- read_sheet(sheets_id, range =  'general_details')
protect_columns_1 <- names(data_study_general) %in% 'doi'

data_study_general <- data_study_general %>%
  rename('notes' = 11, date_published = `date_published_format(2020-05-12)`) %>%
  clean_data(protect = protect_columns_1)
write_rds(data_study_general, 'data_clean/data_study_general.rds')

review_details <- data_study_general %>%
  select(lead_author, date_published, country, review_version)
write_rds(review_details, 'data_clean/review_details.rds')

table_1 <- read_sheet(sheets_id, sheet = 'pop_descriptives') %>%
  clean_data() %>%
  mutate(., lower_range = sub('\\_.*', '', .$range)) %>%
  mutate(., upper_range = sub('.*_', '',.$range,)) %>%
  select(1:9, 22:23, 11:21) %>%
  filter(!is.na(lead_author)) %>%
  left_join(., review_details, by = 'lead_author') %>%
  write_rds(., 'data_clean/data_table_1.rds')
   
table_2 <-  read_sheet(sheets_id, sheet = 'testing') %>%
  clean_data() %>%
  filter(data_on_testing == TRUE) %>%
  left_join(., review_details, by = 'lead_author') %>%
  write_rds(., 'data_clean/data_table_2.rds')

table_3 <-  read_sheet(sheets_id, sheet = 'hospitalisation') %>%
  clean_data() %>%
  filter(data_on_hospitalisation == TRUE) %>%
  left_join(., review_details, by = 'lead_author') %>%
  write_rds(., 'data_clean/data_table_3.rds')

table_4 <-  read_sheet(sheets_id, sheet = 'severity') %>%
  clean_data() %>%
  filter(data_disease_severity == TRUE) %>%
  left_join(., review_details, by = 'lead_author') %>%
  write_rds(., 'data_clean/data_table_4.rds')

table_5 <-  read_sheet(sheets_id, sheet = 'mortality') %>%
  clean_data() %>%
  filter(data_on_deaths == TRUE) %>%
  left_join(., review_details, by = 'lead_author') %>%
  write_rds(., 'data_clean/data_table_5.rds')


table_6 <-  read_sheet(sheets_id, sheet = 'quality_appraisal') %>%
  write_rds(., 'data_clean/data_table_6.rds') 
