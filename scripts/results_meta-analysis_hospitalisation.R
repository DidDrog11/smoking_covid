library(fmsb)
library(epitools)
library(meta)
library(tidyverse)

table_3 <- read_csv('data_clean/table_3_word.csv')
included_studies <- c('argenziano', 'miyara_updated', 'rentsch', 'yanover', 'hamer')

# Data --------------------------------------------------------------------
meta <- tibble('author' = table_3$lead_author,
               'community_smoker' = table_3$community_current_smoker,
               'community_never_smoker' = table_3$community_never_smoker, 
               'hospitalised_smoker' = table_3$hospitalised_current_smoker, 
               'hospitalised_never_smoker' = table_3$hospitalised_never_smoker,
               'community_former_smoker' = table_3$community_former_smoker,
               'hospitalised_former_smoker' = table_3$hospitalised_former_smoker) %>%
  filter(author %in% included_studies)
  
meta$author <- recode(meta$author, 'rentsch' = 'Rentsch',
                      'argenziano' = 'Argenziano',
                      'miyara_updated' = 'Miyara',
                      'yanover' = 'Yanover',
                      'hamer' = 'Hamer')


# Current smoker hospitalisation ------------------------------------------
event_rates_smoker <- meta %>%
  mutate(., Ee = hospitalised_smoker) %>%
  mutate(., Ne = (hospitalised_smoker+community_smoker)) %>%
  mutate(., Ec = hospitalised_never_smoker) %>%
  mutate(., Nc = (hospitalised_never_smoker+community_never_smoker)) %>%
  rename('Author' = author) %>%
  select(Author, Ee, Ne, Ec, Nc)

event_rates_smoker <- metabin(Ee,
                              Ne,
                              Ec,
                              Nc,
                              data = event_rates_smoker,
                              studlab = paste(Author),
                              comb.fixed = F,
                              comb.random = T,
                              method.tau = 'SJ',
                              hakn = T,
                              prediction = F,
                              incr = 0.1,
                              sm = 'RR')
png("figure/fig_4.png", width=1480, height=546, res=120)
current_smoker_hospitalisation <- forest(event_rates_smoker,
                                sortvar = Author,
                                xlim = c(0.5, 3),
                                rightlabs = c('RR', '95% CI', 'Weight'),
                                leftlabs = c('Author', 'logRR', 'SE'),
                                lab.e = 'Current smoker',
                                lab.c = 'Never smoker',
                                print.tau2 = F,
                                col.diamond = 'blue',
                                col.diamond.lines = 'black',
                                col.square = 'black',
                                col.square.lines = 'black',
                                digits.sd = 2)
dev.off()

# Former smoker hospitalisation -------------------------------------------
event_rates_former <- meta %>%
  mutate(., Ee = hospitalised_former_smoker) %>%
  mutate(., Ne = (hospitalised_former_smoker+community_former_smoker)) %>%
  mutate(., Ec = hospitalised_never_smoker) %>%
  mutate(., Nc = (hospitalised_never_smoker+community_never_smoker)) %>%
  rename('Author' = author) %>%
  select(Author, Ee, Ne, Ec, Nc)

event_rates_former <- metabin(Ee,
                              Ne,
                              Ec,
                              Nc,
                              data = event_rates_former,
                              studlab = paste(Author),
                              comb.fixed = F,
                              comb.random = T,
                              method.tau = 'SJ',
                              hakn = T,
                              prediction = F,
                              incr = 0.1,
                              sm = 'RR')
png("figure/fig_5.png", width=1480, height=546, res=120)
former_smoker_hospitalisation <- forest(event_rates_former,
                                         sortvar = Author,
                                         xlim = c(0.5, 5),
                                         rightlabs = c('RR', '95% CI', 'Weight'),
                                         leftlabs = c('Author', 'logRR', 'SE'),
                                         lab.e = 'Former smoker',
                                         lab.c = 'Never smoker',
                                         print.tau2 = F,
                                         col.diamond = 'blue',
                                         col.diamond.lines = 'black',
                                         col.square = 'black',
                                         col.square.lines = 'black',
                                         digits.sd = 2)
dev.off()