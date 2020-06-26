## packages
library(tidyverse)
library(ggsci)
library(matrixStats)
library(gapminder)

## set seed
set.seed(123)

## load fonts
extrafont::loadfonts(device = "win")

## get data
data <-  read_rds(here::here('data_clean', 'country_prevalence_data.rds'))
country_colour <- data.frame(gapminder, cc = I(country_colors[match(gapminder$country, names(country_colors))]))
country_colour <- country_colour %>%
  select(country, cc) %>%
  mutate(country = recode(country,
         "United Kingdom" = "UK",
         "United States" = "USA",
         "Korea, Dem. Rep." = "Korea")) %>%
  rename("country_colour" = cc) %>%
  distinct() %>%
  filter(country %in% data$country) %>%
  deframe()

## sort data
sorted_data <- data %>%
  ungroup() %>%
  mutate(country = fct_reorder(country, -n), ## sorting countries by number of studies from each country
         smoking = fct_relevel(smoking, "former_smoking_p", after = Inf),
         study_setting = recode(study_setting, "quarantine_centre" = "community"),
         study_setting = forcats::fct_explicit_na(study_setting)) %>%
  group_by(smoking, country) %>%
  add_count(name = "prevalence_country") %>%
  mutate(running_count = row_number()) %>%
  group_by(smoking, country, study, study_setting) %>%
  mutate(mean = ifelse(is.na(weightedMean(prevalence, w = true_sample)), mean(prevalence),
                         weightedMean(prevalence, w = true_sample, na.rm = T)),
         prevalence_country = prevalence_country - 1)

sorted_data$running_count <- as_factor(sorted_data$running_count)  

current_sorted_data <- sorted_data %>%
  filter(., smoking == "current_smoking_p") %>%
  arrange(-prevalence, .by_group = T) %>%
  ungroup() %>%
  group_by(country) %>%
  filter(!country %in% c("Brazil", "India", "Portugal", "Turkey"))

a <- ggplot(data = filter(current_sorted_data, study == 1), aes(x = running_count, y = prevalence, colour = country, linetype = study_setting)) +
  scale_colour_manual(values = country_colour, guide = F) +
  geom_point(aes(size = true_sample, shape = factor(study_setting))) + 
  scale_size_continuous(trans = "log10", guide = F) +
  scale_shape_manual(values = c(15, 16, 17), labels = c("Community", "Community & Hospital", "Hospital")) +
  geom_linerange(aes(ymin = lower_ci, ymax =upper_ci), linetype = "solid") +
  geom_segment(data = filter(current_sorted_data, study == 1), aes(x = 0, 
                                                                   xend = prevalence_country + 0.5,
                                                                   y = mean,
                                                                   yend = mean)) +
  scale_linetype_manual(values = c(4, 3, 1), labels = c("Community", "Community & Hospital", "Hospital")) +
  geom_segment(data = filter(current_sorted_data, study == 0), aes(x = 0, 
                                                                   xend = prevalence_country + 0.5,
                                                                   y = prevalence,
                                                                   yend = prevalence), linetype = "solid",
               size = 0.8) +
  scale_y_continuous(name = "Prevalence", limits = c(0, 0.7), breaks = scales::pretty_breaks(n = 9), expand = c(0, 0)) +
  coord_flip() +
  facet_grid(country ~  ., scales = "free_y", space = "free", switch = 'y') +
  theme(panel.spacing = unit(0.1, "lines"),
        strip.text = element_text(angle = 0, size = 12),
        axis.title = element_text(size = 10),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 10),
        panel.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        strip.text.y.left = element_text(angle = 0),
        legend.position = "right") +
  labs(title ="Prevalence of current smoking in included studies",
       linetype = "Weighted mean prevalence by study setting", shape = "Study setting")
a

png(here::here('reports', 'figure', 'figure_2a.png'), width=912, height=970, res = 120)
a
null <- dev.off()


former_sorted_data <- sorted_data %>%
  filter(., smoking == "former_smoking_p") %>%
  filter(., prevalence_country != 0 ) %>%
  arrange(-prevalence, .by_group = T)

a <- ggplot(data = filter(former_sorted_data, study == 1), aes(x = running_count, y = prevalence, colour = country,  linetype = study_setting)) +
  scale_colour_manual(values = country_colour, guide = F) +
  geom_point(aes(size = true_sample, shape = factor(study_setting))) + 
  scale_size_continuous(trans = "log10", guide = F) +
  scale_shape_manual(values = c(15, 16, 17), labels = c("Community", "Community & Hospital", "Hospital")) +
  geom_linerange(aes(ymin = lower_ci, ymax =upper_ci), linetype = "solid") +
  geom_segment(data = filter(former_sorted_data, study == 1), aes(x = 0, 
                                                                   xend = prevalence_country + 0.5,
                                                                   y = mean,
                                                                   yend = mean)) +
  scale_linetype_manual(values = c(4, 3, 1), labels = c("Community", "Community & Hospital", "Hospital")) +
  geom_segment(data = filter(former_sorted_data, study == 0), aes(x = 0, 
                                                                   xend = prevalence_country + 0.5,
                                                                   y = prevalence,
                                                                   yend = prevalence),
               linetype = "solid",
               size = 0.8) +
  scale_y_continuous(name = "Prevalence", limits = c(0, 0.9), breaks = scales::pretty_breaks(n = 9), expand = c(0, 0)) +
  coord_flip() +
  facet_grid(country ~  ., scales = "free_y", space = "free", switch = 'y') +
  theme(panel.spacing = unit(0.1, "lines"),
        strip.text = element_text(angle = 0, size = 12),
        axis.title = element_text(size = 10),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 10),
        panel.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        strip.text.y.left = element_text(angle = 0),
        legend.position = "right") +
  labs(title ="Prevalence of former smoking in included studies",
       linetype = "Weighted mean prevalence by study setting", shape = "Study setting")
a

png(here::here('reports', 'figure', 'figure_2b.png'), width=912, height=967, res = 120)
a
null <- dev.off()
