library(tidyverse)
library(naniar)
library(gridExtra)
library(snakecase)

general <- read_rds('data_clean/data_study_general.rds')
table_1 <- read_rds('data_clean/data_table_1.rds') %>%
  filter(review_version != 'v4') %>%
  filter(lead_author != 'miyara_old')

smoking_status <- c('current_smoker', 'former_smoker', 'current_former_smoker', 'never_smoker', 'never_smoker_unknown', 'not_stated', 'missing')


prevalence_plot <- table_1 %>%
  group_by(country) %>%
  rename('sample' = total) %>%
  filter(current_smoker != 'NA') %>%
  replace_na(., list(current_smoker = 0,
                     former_smoker = 0,
                     current_former_smoker = 0,
                     never_smoker = 0,
                     never_smoker_unknown = 0,
                     not_stated = 0,
                     missing = 0)) %>%
  mutate(ever_smoker = former_smoker + current_smoker + current_former_smoker,
         not_stated_missing = missing + not_stated + never_smoker_unknown) %>%
  select(lead_author, country, sample, current_smoker, former_smoker, never_smoker, ever_smoker, not_stated_missing) %>%
  mutate(total = never_smoker + ever_smoker + not_stated_missing) %>%
  mutate(p_current_smoker = current_smoker/total,
         p_ever_smoker = ever_smoker/total,
         p_former_smoker = former_smoker/total,
         p_never_smoker = never_smoker/total,
         p_ever_smoker = ever_smoker/total,
         p_not_stated_missing = not_stated_missing/total,
         p_total = p_ever_smoker + p_never_smoker + p_not_stated_missing) %>%
  mutate(study = 1) %>%
  add_count(country) 


prevalence_plot$country <- as.factor(prevalence_plot$country)

a <- tibble(country = c('china', 'usa', 'uk', 'france', 'mexico', 'spain', 'italy', 'iran', 'israel', 'korea', 'kuwait', 'switzerland'),
            current_smoking_p = c(0.277, 0.138, 0.144, 0.32, 0.166, 0.276, 0.19, 0.101, 0.22, 0.193, 0.225, 0.25),
            former_smoking_p = c(0.04, 0.209, 0.258, 0.314, 0, 0, 0.234, 0, 0, 0, 0, 0),
            study = 0)
country_list_ordered <- c('China', 'USA', 'UK', 'France', 'Italy', 'Israel', 'Mexico', 'Spain', 'Iran', 'Korea', 'Kuwait', 'Switzerland')

b <- prevalence_plot %>%
  ungroup() %>%
  mutate(current_smoking_p = p_current_smoker,
         former_smoking_p = p_former_smoker) %>%
  add_row(country = a$country, current_smoking_p = a$current_smoking_p, former_smoking_p = a$former_smoking_p, study = a$study) %>%
  group_by(country) %>%
  filter(country != 'multiple') %>%
  select(country, sample, study, current_smoking_p, former_smoking_p) %>%
  add_count(country)

b$study <- as.factor(b$study)

c <- b %>%
  pivot_longer(., c(current_smoking_p, former_smoking_p), names_to = 'smoking', values_to = 'prevalence') %>%
  filter(prevalence != 0) %>%
  select(-n) %>%
  add_count(country)

c$country <- c$country %>%
  to_upper_camel_case() %>%
  recode('Usa' = 'USA', 'Uk' = 'UK')

ggplot(c, aes(x = smoking, y = prevalence, fill = study))+
  geom_dotplot(binaxis = 'y', method = 'histodot', stackdir = 'center', binpositions = 'all', dotsize = 1, stackgroups = T)+
  facet_wrap(~reorder(country, -n), nrow = 2)+
  labs(title = 'Smoking prevalence in included studies compared with national prevalence', y = 'Prevalence', fill = '')+
  scale_fill_discrete(labels = c('National prevalence', 'Study population prevalence'))+
  scale_x_discrete(name = 'Smoking status', labels = c('Current', 'Former'))+
  theme_bw()+
  theme(legend.position = 'bottom')

