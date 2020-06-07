## packages
library(tidyverse)
library(ggsci)
library(matrixStats)

## set seed
set.seed(123)

## load fonts
extrafont::loadfonts(device = "win")

## get data
data <-  read_rds(here::here('data_clean', 'country_prevalence_data.rds'))

## sort data
sorted_data <- data %>%
  ungroup() %>%
  mutate(country = fct_reorder(country, -n), ## sorting countries by number of studies from each country
         smoking = fct_relevel(smoking, "former_smoking_p", after = Inf)) %>%
  group_by(smoking, country) %>%
  add_count(name = "prevalence_country") %>%
  group_by(smoking, country, study) %>%
  mutate(mean = ifelse(is.na(weightedMean(prevalence, w = true_sample, idxs = c(smoking, country, study))), mean(prevalence),
                         weightedMean(prevalence, w = true_sample, idxs = c(smoking, country, study))),
         running_count = row_number(),
         prevalence_country = prevalence_country - 1)

sorted_data$running_count <- as_factor(sorted_data$running_count)  

current_sorted_data <- sorted_data %>%
  filter(., smoking == "current_smoking_p") %>%
  arrange(-prevalence, .by_group = T)

a <- ggplot(data = filter(current_sorted_data, study == 1), aes(x = running_count, y = prevalence, colour = country)) +
  geom_point() +
  geom_linerange(aes(ymin = lower_ci, ymax =upper_ci)) +
  geom_segment(data = filter(current_sorted_data, study == 1), aes(x = 0, 
                                                                   xend = prevalence_country + 0.5,
                                                                   y = mean,
                                                                   yend = mean),
               linetype = "dashed") +
  geom_segment(data = filter(current_sorted_data, study == 0), aes(x = 0, 
                                                                   xend = prevalence_country + 0.5,
                                                                   y = prevalence,
                                                                   yend = prevalence),
               size = 0.8) +
  scale_y_continuous(name = "Prevalence", limits = c(0, 0.7), breaks = scales::pretty_breaks(n = 9), expand = c(0, 0)) +
  coord_flip() +
  facet_grid(country ~  ., scales = "free_y", space = "free") +
  theme(panel.spacing = unit(0, "lines"),
        strip.text.y = element_text(angle = 0),
        axis.title = element_text(size = 10),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 10),
        panel.background = element_blank(),
        strip.background = element_rect(colour = "black", fill = "white", 
                                          size = 0.5, linetype = "solid"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.2),
        axis.line = element_line(colour = "black"),
        legend.position = "none") +
  labs(title ="Prevalence of current smoking in included studies")
a

png(here::here('reports', 'figure', 'figure_2a.png'), width=739, height=784, res = 120)
a
null <- dev.off()


former_sorted_data <- sorted_data %>%
  filter(., smoking == "former_smoking_p") %>%
  filter(., prevalence_country != 0 ) %>%
  arrange(-prevalence, .by_group = T)

a <- ggplot(data = filter(former_sorted_data, study == 1), aes(x = running_count, y = prevalence, colour = country)) +
  geom_point() +
  geom_linerange(aes(ymin = lower_ci, ymax =upper_ci)) +
  geom_segment(data = filter(former_sorted_data, study == 1), aes(x = 0, 
                                                                   xend = prevalence_country + 0.5,
                                                                   y = mean,
                                                                   yend = mean),
               linetype = "dashed") +
  geom_segment(data = filter(former_sorted_data, study == 0), aes(x = 0, 
                                                                   xend = prevalence_country + 0.5,
                                                                   y = prevalence,
                                                                   yend = prevalence),
               size = 0.8) +
  scale_y_continuous(name = "Prevalence", limits = c(0, 0.7), breaks = scales::pretty_breaks(n = 9), expand = c(0, 0)) +
  coord_flip() +
  facet_grid(country ~  ., scales = "free_y", space = "free") +
  theme(panel.spacing = unit(0, "lines"),
        strip.text.y = element_text(angle = 0),
        axis.title = element_text(size = 10),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 10),
        panel.background = element_blank(),
        strip.background = element_rect(colour = "black", fill = "white", 
                                        size = 0.5, linetype = "solid"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.2),
        axis.line = element_line(colour = "black"),
        legend.position = "none") +
  labs(title ="Prevalence of former smoking in included studies")
a

png(here::here('reports', 'figure', 'figure_2b.png'), width=739, height=784, res = 120)
a
null <- dev.off()
