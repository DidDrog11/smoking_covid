library(tidyverse)
library(here)
library(linelist)
library(boot)
library(googlesheets4)

source(here("scripts", "bootstrap_function.R"))

set.seed(42)
sheets_id <- as_sheets_id('https://docs.google.com/spreadsheets/d/15avypGR8ypJngWQEmFIzFrOOwXPY3xezUHQU6jgV7d0/edit?usp=sharing')
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
  filter(country != 'multiple') %>%
  mutate(study_id = 1:nrow(.)) %>%
  mutate(current_smoking_p = p_current_smoker,
         former_smoking_p = p_former_smoker,
         true_sample = sample) %>%
  add_row(country = national_smoking_prevalence$country,
          current_smoking_p = national_smoking_prevalence$current_smoking_p, 
          former_smoking_p = national_smoking_prevalence$former_smoking_p, 
          study = national_smoking_prevalence$study) %>%
  select(country, sample, study, current_smoking_p, former_smoking_p, study_id, true_sample) %>%
  add_count(country) %>%
  group_by(country, study_id)


b$study <- as.factor(b$study)
b$sample[bootstrap$sample >= 50000] <- 50000
bootstrap <- b %>%
  filter(study != 0) %>%
  select(study_id, sample, current_smoking_p, former_smoking_p) %>%
  ungroup() %>%
  mutate(bs_current = sample*current_smoking_p,
         bs_inverse = sample*(1-current_smoking_p),
         bs_former = sample*former_smoking_p,
         bs_inverse_former = sample*(1-former_smoking_p))



# Bootstrap ---------------------------------------------------------------

bootstrap_output_c <- list()
for(i in 1:length(bootstrap$study_id)) {
  bootstrap_output_c <- do_bootstrap_current(i)
}

bootstrap_current <- do.call(rbind.data.frame, bootstrap_output_c) %>%
  rename("lower_CI_current" = 1,
         "upper_CI_current" = 2) %>%
  mutate(study_id = 1:nrow(.),
         smoking = "current_smoking_p")

bootstrap_output_f <- list()
for(i in 1:length(bootstrap$study_id)) {
  bootstrap_output_f <- do_bootstrap_former(i)
}

bootstrap_former <- do.call(rbind.data.frame, bootstrap_output_f) %>%
  rename("lower_CI_former" = 1,
         "upper_CI_former" = 2) %>%
  mutate(study_id = 1:nrow(.),
         smoking = "former_smoking_p")

bootstrap_ci <- bootstrap_current %>%
  full_join(., bootstrap_former, by = c("study_id", "smoking")) %>%
  select(study_id, lower_CI_current, upper_CI_current, lower_CI_former, upper_CI_former, smoking) %>%
  mutate(lower_ci = ifelse(is.na(lower_CI_current), lower_CI_former, lower_CI_current),
         upper_ci = ifelse(is.na(upper_CI_current), upper_CI_former, upper_CI_current)) %>%
  select(study_id, smoking, lower_ci, upper_ci)
 
c <- b %>%
  pivot_longer(., c(current_smoking_p, former_smoking_p), 
               names_to = 'smoking', values_to = 'prevalence') %>%
  left_join(., bootstrap_ci, by = c("study_id", "smoking")) %>%
  filter(prevalence != 0)

c$country <- c$country %>%
  snakecase::to_upper_camel_case() %>%
  recode('Usa' = 'USA', 'Uk' = 'UK', "SaudiArabia" = "Saudi Arabia")

write_rds(c, here::here('data_clean', 'country_prevalence_data.rds'))

d <- ggplot(c, aes(x = smoking, y = prevalence, fill = study, ymin = lower_ci, ymax = upper_ci))+
  geom_dotplot(binaxis = 'y', method = 'histodot', stackdir = 'center', binpositions = 'all', dotsize = 0.8, stackgroups = T)+
  geom_linerange() +
  facet_wrap(~reorder(country, -n), nrow = 2)+
  labs(title = 'Smoking prevalence in included studies compared with national prevalence', y = 'Prevalence', fill = '')+
  scale_fill_discrete(labels = c('National prevalence', 'Study population prevalence'))+
  scale_x_discrete(name = 'Smoking status', labels = c('Current', 'Former'))+
  theme_bw()+
  theme(legend.position = 'bottom')

d

png(here::here('reports', 'figure', 'fig_2.png'), width=1024, height=546, res=120)
d
null <- dev.off()

e <- ggplot(c, aes(x = smoking, y = prevalence, fill = study))+
  geom_dotplot(binaxis = 'y', method = 'histodot', stackdir = 'center', binpositions = 'all', dotsize = 1, stackgroups = T)+
  facet_wrap(~reorder(country, -n), nrow = 2)+
  labs(title = 'Smoking prevalence in included studies compared with national prevalence', y = 'Prevalence', fill = '')+
  scale_fill_discrete(labels = c('National prevalence', 'Study population prevalence'))+
  scale_x_discrete(name = 'Smoking status', labels = c('Current', 'Former'))+
  theme_bw()+
  theme(legend.position = 'bottom')

png(here::here('reports', 'figure', 'fig_2b.png'), width=1024, height=546, res=120)
e
null <- dev.off()
