library(epitools)
library(meta)
library(tidyverse)

table_4 <- read_csv('data_clean/table_4_word.csv')
included_studies <- c('guan_ni', 'hadjadj', 'rentsch')

# Data --------------------------------------------------------------------
meta <- tibble('author' = table_4$lead_author,
               'non_severe_smoker' = table_4$non_severe_current_smoker,
               'non_severe_never_smoker' = table_4$non_severe_never_smoker,
               'severe_smoker' = table_4$severe_disease_current_smoker,
               'severe_never_smoker' = table_4$severe_disease_never_smoker,
               'non_severe_former_smoker' = table_4$non_severe_former_smoker,
               'severe_former_smoker' = table_4$severe_disease_former_smoker) %>%
  filter(author %in% included_studies)

meta$author <- recode(meta$author, 'rentsch' = 'Rentsch',
                      'guan_ni' = 'Guan, Ni',
                      'hadjadj' = 'Hadjadj')


# Current smoker severity ------------------------------------------
event_rates_smoker <- meta %>%
  mutate(., Ee = severe_smoker) %>%
  mutate(., Ne = (severe_smoker+non_severe_smoker)) %>%
  mutate(., Ec = severe_never_smoker) %>%
  mutate(., Nc = (severe_never_smoker+non_severe_never_smoker)) %>%
  rename('Author' = author) %>%
  select(Author, Ee, Ne, Ec, Nc)

event_rates_smoker <- metabin(Ee,
                              Ne,
                              Ec,
                              Nc,
                              data = event_rates_smoker,
                              studlab = paste(Author),
                              comb.fixed = T,
                              comb.random = F,
                              method.tau = 'SJ',
                              hakn = F,
                              prediction = F,
                              incr = 0.1,
                              sm = 'RR')
png("figure/fig_6.png", width=1024, height=546, res=120)
current_smoker_severity <- forest(event_rates_smoker,
                                         sortvar = Author,
                                         xlim = c(0.1, 5),
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
  mutate(., Ee = severe_former_smoker) %>%
  mutate(., Ne = (severe_former_smoker+non_severe_former_smoker)) %>%
  mutate(., Ec = severe_never_smoker) %>%
  mutate(., Nc = (severe_never_smoker+non_severe_never_smoker)) %>%
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
                              hakn = F,
                              prediction = F,
                              incr = 0.1,
                              sm = 'RR')

png("figure/fig_7.png", width=1024, height=546, res=120)
former_smoker_severity <- forest(event_rates_former,
                                        sortvar = Author,
                                        xlim = c(0.2, 10),
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