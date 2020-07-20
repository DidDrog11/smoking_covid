library(brms)
library(ggplot2)
library(tidybayes)
library(readr)
library(dplyr)
library(ggridges)
library(glue)
library(stringr)
library(forcats)
library(here)
library(meta)

source(here("scripts", "bayes_scripts.R"))
previous_review_versions <- c("v1", "v2", "v3", "v4", "v5")
current_review_version <- "v6"

study_review_version <- data_study_general %>%
  mutate(lead_author = to_upper_camel_case(lead_author, sep_out = ", "),
         lead_author = plyr::mapvalues(lead_author,
                                 from = cleaned_names,
                                 to = correct_names),
         lead_author = recode(lead_author, "de Lusignan" = "de.Lusignan")) %>%
  select(lead_author, review_version) %>%
  rename(Author = lead_author) %>%
  mutate(review_version = factor(fct_collapse(review_version, previous = previous_review_versions, current = current_review_version)))

levels(study_review_version$review_version) <- c(levels(study_review_version$review_version), "combined_pooled") %>%
  fct_relevel(c("previous", "current", "combined_pooled"))


bayes_testing_current <-
  read_rds(here("data_clean", "bayes_testing_current.rds"))
bayes_testing_former <-
  read_rds(here("data_clean", "bayes_testing_former.rds"))
bayes_hospital_current <-
  read_rds(here("data_clean", "bayes_hospital_current.rds"))
bayes_hospital_former <-
  read_rds(here("data_clean", "bayes_hospital_former.rds"))
bayes_severity_current <-
  read_rds(here("data_clean", "bayes_severity_current.rds"))
bayes_severity_former <-
  read_rds(here("data_clean", "bayes_severity_former.rds"))
bayes_mortality_current <-
  read_rds(here("data_clean", "bayes_mortality_current.rds"))
bayes_mortality_former <-
  read_rds(here("data_clean", "bayes_mortality_former.rds"))

testing_bayes_c <- extract_TE(bayes_testing_current)
testing_bayes_f <- extract_TE(bayes_testing_former)
hospital_bayes_c <- extract_TE(bayes_hospital_current)
hospital_bayes_f <- extract_TE(bayes_hospital_former)
severity_bayes_c <- extract_TE(bayes_severity_current)
severity_bayes_f <- extract_TE(bayes_severity_former)
mortality_bayes_c <- extract_TE(bayes_mortality_current)
mortality_bayes_f <- extract_TE(bayes_mortality_former)

minimally_informative_prior <-
  c(prior(normal(0, 1), class = Intercept),
    prior(cauchy(0, 0.5), class = sd))

# Classical meta-analysis guided priors
current_testing_prior <-
  c(prior(normal(-.15, 1), class = Intercept),
    prior(cauchy(0, 1), class = sd))
former_testing_prior <-
  c(prior(normal(0.009, 1), class = Intercept),
    prior(cauchy(0, 1), class = sd))
current_hospitalisation_prior <-
  c(prior(normal(0.06, 1), class = Intercept),
    prior(cauchy(0, 1), class = sd)) #0.06 is ln(1.06311)
former_hospitalisation_prior <-
  c(prior(normal(0.18, 1), class = Intercept),
    prior(cauchy(0, 1), class = sd)) #0.18 is ln(1.1997)
current_severity_prior <-
  c(prior(normal(0.2, 1), class = Intercept),
    prior(cauchy(0, 1), class = sd)) #0.2 is ln(1.2214)
former_severity_prior <-
  c(prior(normal(0.46, 1), class = Intercept),
    prior(cauchy(0, 1), class = sd)) #0.46 is ln(1.5775)
current_mortality_prior <-
  c(prior(normal(0.53, 1), class = Intercept),
    prior(cauchy(0, 1), class = sd)) #0.53 is ln(1.704)
former_mortality_prior <- 
  c(prior(normal(0.69, 1), class = Intercept),
    prior(cauchy(0, 1), class = sd)) #0.69 is ln(2.0004)

# Current smokers testing -------------------------------------------------
# Bayesian analysis for current smokers and testing for SARS-CoV-2
m1 <-
  bayes_test(testing_bayes_c, minimally_informative_prior, iterations = 4000)
pp_check(m1)
summary(m1)
ranef(m1)

m1_a <-
  bayes_test(testing_bayes_c, current_testing_prior, iterations = 4000)
pp_check(m1_a)
summary(m1_a)
ranef(m1_a)

post_samples_m1 <- post_samples(m1)
post_samples_m1_a <- post_samples(m1_a)

#rr_ecdf <- ecdf(post_samples_m1$TE)
#rr_ecdf(0.06)

study_draws_m1 <- study_draw(m1)
pooled_draw_m1 <- pooled_effect_draw(m1)

study_draws_m1_a <- study_draw(m1_a)
pooled_draw_m1_a <- pooled_effect_draw(m1_a)

current_testing_minimal_prior <- forest_plot(
  m1,
  study_draws_m1,
  pooled_draw_m1,
  "Forest plot of current smokers and risk of testing positive",
  "Minimally informative prior",
  "m1.png"
)

current_testing_alternative_prior <- forest_plot(
  m1_a,
  study_draws_m1_a,
  pooled_draw_m1_a,
  "Forest plot of current smokers and risk of testing positive",
  "Current testing prior",
  "m1_a.png"
)

# Former smokers testing --------------------------------------------------
# Bayesian analysis for former smokers and testing for SARS-CoV-2
m2 <-
  bayes_test(testing_bayes_f, minimally_informative_prior, iterations = 4000)
pp_check(m2)
summary(m2)
ranef(m2)

m2_a <-
  bayes_test(testing_bayes_f, former_testing_prior, iterations = 4000)
pp_check(m2_a)
summary(m2_a)
ranef(m2_a)

post_samples_m2 <- post_samples(m2)
post_samples_m2_a <- post_samples(m2_a)

#rr_ecdf <- ecdf(post_samples_m2$TE)
#rr_ecdf(0.06)

study_draws_m2 <- study_draw(m2)
pooled_draw_m2 <- pooled_effect_draw(m2)

study_draws_m2_a <- study_draw(m2_a)
pooled_draw_m2_a <- pooled_effect_draw(m2_a)

former_testing_minimal_prior <- forest_plot(
  m2,
  study_draws_m2,
  pooled_draw_m2,
  "Forest plot of former smokers and risk of testing positive",
  "Minimally informative prior",
  "m2.png"
)
former_testing_alternative_prior <- forest_plot(
  m2_a,
  study_draws_m2_a,
  pooled_draw_m2_a,
  "Forest plot of former smokers and risk of testing positive",
  "Current testing prior",
  "m2_a.png"
)

# Current smokers hospitalisation --------------------------------------------------
# Bayesian analysis for current smokers and hospitalisation for SARS-CoV-2
m3 <-
  bayes_test(hospital_bayes_c, minimally_informative_prior, iterations = 4000)
pp_check(m3)
summary(m3)
ranef(m3)

m3_a <-
  bayes_test(hospital_bayes_c, current_hospitalisation_prior, iterations = 4000)
pp_check(m3_a)
summary(m3_a)
ranef(m3_a)

post_samples_m3 <- post_samples(m3)
post_samples_m3_a <- post_samples(m3_a)

#rr_ecdf <- ecdf(post_samples_m3$TE)
#rr_ecdf(0.06)

study_draws_m3 <- study_draw(m3)
pooled_draw_m3 <- pooled_effect_draw(m3)

study_draws_m3_a <- study_draw(m3_a)
pooled_draw_m3_a <- pooled_effect_draw(m3_a)

current_hospitalisation_minimal_prior <- forest_plot(
  m3,
  study_draws_m3,
  pooled_draw_m3,
  "Forest plot of current smokers and risk of hospital admission",
  "Minimally informative prior",
  "m3.png"
)
current_hospitalisation_alternative_prior <- forest_plot(
  m3_a,
  study_draws_m3_a,
  pooled_draw_m3_a,
  "Forest plot of current smokers and risk of hospital admission",
  "Current hospitalisation prior",
  "m3_a.png"
)

# Former smokers hospitalisation --------------------------------------------------
# Bayesian analysis for former smokers and hospitalisation for SARS-CoV-2
m4 <-
  bayes_test(hospital_bayes_f, minimally_informative_prior, iterations = 4000)
pp_check(m4)
summary(m4)
ranef(m4)

m4_a <-
  bayes_test(hospital_bayes_f, former_hospitalisation_prior, iterations = 4000)
pp_check(m4_a)
summary(m4_a)
ranef(m4_a)

post_samples_m4 <- post_samples(m4)
post_samples_m4_a <- post_samples(m4_a)

#rr_ecdf <- ecdf(post_samples_m4$TE)
#rr_ecdf(0.06)

study_draws_m4 <- study_draw(m4)
pooled_draw_m4 <- pooled_effect_draw(m4)

study_draws_m4_a <- study_draw(m4_a)
pooled_draw_m4_a <- pooled_effect_draw(m4_a)

former_hospitalisation_minimal_prior <- forest_plot(
  m4,
  study_draws_m4,
  pooled_draw_m4,
  "Forest plot of former smokers and risk of hospital admission",
  "Minimally informative prior",
  "m4.png"
)
former_hospitalisation_alternative_prior <- forest_plot(
  m4_a,
  study_draws_m4_a,
  pooled_draw_m4_a,
  "Forest plot of former smokers and risk of hospital admission",
  "Former hospitalisation prior",
  "m4_a.png"
)


# Current smokers disease severity --------------------------------------------------
# Bayesian analysis for current smokers and severity for SARS-CoV-2
m5 <-
  bayes_test(severity_bayes_c, minimally_informative_prior, iterations = 4000)
pp_check(m5)
summary(m5)
ranef(m5)

m5_a <-
  bayes_test(severity_bayes_c, current_severity_prior, iterations = 4000)
pp_check(m5_a)
summary(m5_a)
ranef(m5_a)

post_samples_m5 <- post_samples(m5)
post_samples_m5_a <- post_samples(m5_a)

#rr_ecdf <- ecdf(post_samples_m5$TE)
#rr_ecdf(0.06)

study_draws_m5 <- study_draw(m5)
pooled_draw_m5 <- pooled_effect_draw(m5)

study_draws_m5_a <- study_draw(m5_a)
pooled_draw_m5_a <- pooled_effect_draw(m5_a)

current_severity_minimal_prior <- forest_plot(
  m5,
  study_draws_m5,
  pooled_draw_m5,
  "Forest plot of current smokers and risk of severe disease",
  "Minimally informative prior",
  "m5.png"
)
current_severity_alternative_prior <- forest_plot(
  m5_a,
  study_draws_m5_a,
  pooled_draw_m5_a,
  "Forest plot of current smokers and risk of severe disease",
  "Current severity prior",
  "m5_a.png"
)

# Former smokers disease severity --------------------------------------------------
# Bayesian analysis for current smokers and severity for SARS-CoV-2
m6 <-
  bayes_test(severity_bayes_f, minimally_informative_prior, iterations = 4000)
pp_check(m6)
summary(m6)
ranef(m6)

m6_a <-
  bayes_test(severity_bayes_f, former_severity_prior, iterations = 4000)
pp_check(m6_a)
summary(m6_a)
ranef(m6_a)

post_samples_m6 <- post_samples(m6)
post_samples_m6_a <- post_samples(m6_a)

#rr_ecdf <- ecdf(post_samples_m6$TE)
#rr_ecdf(0.06)

study_draws_m6 <- study_draw(m6)
pooled_draw_m6 <- pooled_effect_draw(m6)

study_draws_m6_a <- study_draw(m6_a)
pooled_draw_m6_a <- pooled_effect_draw(m6_a)

former_severity_minimal_prior <- forest_plot(
  m6,
  study_draws_m6,
  pooled_draw_m6,
  "Forest plot of former smokers and risk of severe disease",
  "Minimally informative prior",
  "m6.png"
)
former_severity_alternative_prior <- forest_plot(
  m6_a,
  study_draws_m6_a,
  pooled_draw_m6_a,
  "Forest plot of former smokers and risk of severe disease",
  "Former severity prior",
  "m6_a.png"
)

# Current smokers mortality --------------------------------------------------
# Bayesian analysis for current smokers and mortality from SARS-CoV-2
m7 <-
  bayes_test(mortality_bayes_c, minimally_informative_prior, iterations = 4000)
pp_check(m7)
summary(m7)
ranef(m7)

m7_a <-
  bayes_test(mortality_bayes_c, current_mortality_prior, iterations = 4000)
pp_check(m7_a)
summary(m7_a)
ranef(m7_a)

post_samples_m7 <- post_samples(m7)
post_samples_m7_a <- post_samples(m7_a)

#rr_ecdf <- ecdf(post_samples_m7$TE)
#rr_ecdf(0.06)

study_draws_m7 <- study_draw(m7)
pooled_draw_m7 <- pooled_effect_draw(m7)

study_draws_m7_a <- study_draw(m7_a)
pooled_draw_m7_a <- pooled_effect_draw(m7_a)

current_mortality_minimal_prior <- forest_plot(
  m7,
  study_draws_m7,
  pooled_draw_m7,
  "Forest plot of current smokers and mortality",
  "Minimally informative prior",
  "m7.png"
)
current_mortality_alternative_prior <- forest_plot(
  m7_a,
  study_draws_m7_a,
  pooled_draw_m7_a,
  "Forest plot of current smokers and mortality",
  "Current mortality prior",
  "m7.png"
)

# Former smokers mortality --------------------------------------------------
# Bayesian analysis for former smokers and mortality from SARS-CoV-2
m8 <-
  bayes_test(mortality_bayes_f, minimally_informative_prior, iterations = 4000)
pp_check(m8)
summary(m8)
ranef(m8)

m8_a <-
  bayes_test(mortality_bayes_f, former_mortality_prior, iterations = 4000)
pp_check(m8_a)
summary(m8_a)
ranef(m8_a)

post_samples_m8 <- post_samples(m8)
post_samples_m8_a <- post_samples(m8_a)

#rr_ecdf <- ecdf(post_samples_m8$TE)
#rr_ecdf(0.06)

study_draws_m8 <- study_draw(m8)
pooled_draw_m8 <- pooled_effect_draw(m8)

study_draws_m8_a <- study_draw(m8_a)
pooled_draw_m8_a <- pooled_effect_draw(m8_a)

former_mortality_minimal_prior <- forest_plot(
  m8,
  study_draws_m8,
  pooled_draw_m8,
  "Forest plot of former smokers and mortality",
  "Minimally informative prior",
  "m8.png"
)
former_mortality_alternative_prior <- forest_plot(
  m8_a,
  study_draws_m8_a,
  pooled_draw_m8_a,
  "Forest plot of former smokers and mortality",
  "Former mortality prior",
  "m8_a.png"
)
