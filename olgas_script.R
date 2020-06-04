library(tidyverse)
library(here)
library(linelist)

smoking_status <- c('current_smoker', 'former_smoker', 'current_former_smoker', 'never_smoker', 'never_smoker_unknown', 'not_stated', 'missing')
table_1 <- read_rds(here("data_clean", "table_1.rds"))

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

google_smoking_prevalence <-  read_sheet(sheets_id, sheet = 'national_smoking_prevalence') 
national_smoking_prevalence <- google_smoking_prevalence %>%
  select(-Source) %>%
  mutate(study = 0,
         Current = Current/100,
         Former = Former/100) %>%
  rename("country" = Country,
         "current_smoking_p" = Current,
         "former_smoking_p" = Former) %>%
  clean_data()


country_list_ordered <- sort(national_smoking_prevalence$country)

b <- prevalence_plot %>%
  ungroup() %>%
  mutate(current_smoking_p = p_current_smoker,
         former_smoking_p = p_former_smoker) %>%
  add_row(country = national_smoking_prevalence$country,
          current_smoking_p = national_smoking_prevalence$current_smoking_p, 
          former_smoking_p = national_smoking_prevalence$former_smoking_p, 
          study = national_smoking_prevalence$study) %>%
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
  recode('Usa' = 'USA', 'Uk' = 'UK', "SaudiArabia" = "Saudi Arabia")

d <- ggplot(c, aes(x = smoking, y = prevalence, fill = study))+
  geom_dotplot(binaxis = 'y', method = 'histodot', stackdir = 'center', binpositions = 'all', dotsize = 1, stackgroups = T)+
  facet_wrap(~reorder(country, -n), nrow = 2)+
  labs(title = 'Smoking prevalence in included studies compared with national prevalence', y = 'Prevalence', fill = '')+
  scale_fill_discrete(labels = c('National prevalence', 'Study population prevalence'))+
  scale_x_discrete(name = 'Smoking status', labels = c('Current', 'Former'))+
  theme_bw()+
  theme(legend.position = 'bottom')

d
