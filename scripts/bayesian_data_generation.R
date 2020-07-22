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
set.seed(42)
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
  c(prior(normal(0.34, 1), class = Intercept),
    prior(cauchy(0, 1), class = sd)) #0.34 is ln(1.41) result from v4
former_mortality_prior <- 
  c(prior(normal(-0.02, 1), class = Intercept),
    prior(cauchy(0, 1), class = sd)) #0.0.02 is ln(0.98) result from v4


# Run models --------------------------------------------------------------

##Testing current
m1 <-
  bayes_test(testing_bayes_c, minimally_informative_prior, iterations = 40000)
pp_check(m1)
summary(m1)
ranef(m1)

m1_a <-
  bayes_test(testing_bayes_c, current_testing_prior, iterations = 40000)
pp_check(m1_a)
summary(m1_a)
ranef(m1_a)

##Testing former
m2 <-
  bayes_test(testing_bayes_f, minimally_informative_prior, iterations = 40000)
pp_check(m2)
summary(m2)
ranef(m2)

m2_a <-
  bayes_test(testing_bayes_f, former_testing_prior, iterations = 40000)
pp_check(m2_a)
summary(m2_a)
ranef(m2_a)

##Hospitalisation current
m3 <-
  bayes_test(hospital_bayes_c, minimally_informative_prior, iterations = 40000)
pp_check(m3)
summary(m3)
ranef(m3)

m3_a <-
  bayes_test(hospital_bayes_c, current_hospitalisation_prior, iterations = 40000)
pp_check(m3_a)
summary(m3_a)
ranef(m3_a)

##Hospitalisation former
m4 <-
  bayes_test(hospital_bayes_f, minimally_informative_prior, iterations = 40000)
pp_check(m4)
summary(m4)
ranef(m4)

m4_a <-
  bayes_test(hospital_bayes_f, former_hospitalisation_prior, iterations = 40000)
pp_check(m4_a)
summary(m4_a)
ranef(m4_a)

##Severity current
m5 <-
  bayes_test(severity_bayes_c, minimally_informative_prior, iterations = 40000)
pp_check(m5)
summary(m5)
ranef(m5)

m5_a <-
  bayes_test(severity_bayes_c, current_severity_prior, iterations = 40000)
pp_check(m5_a)
summary(m5_a)
ranef(m5_a)

##Severity former
m6 <-
  bayes_test(severity_bayes_f, minimally_informative_prior, iterations = 40000)
pp_check(m6)
summary(m6)
ranef(m6)

m6_a <-
  bayes_test(severity_bayes_f, former_severity_prior, iterations = 40000)
pp_check(m6_a)
summary(m6_a)
ranef(m6_a)

##Mortality current
m7 <-
  bayes_test(mortality_bayes_c, minimally_informative_prior, iterations = 40000)
pp_check(m7)
summary(m7)
ranef(m7)

m7_a <-
  bayes_test(mortality_bayes_c, current_mortality_prior, iterations = 40000)
pp_check(m7_a)
summary(m7_a)
ranef(m7_a)

##Mortality former
m8 <-
  bayes_test(mortality_bayes_f, minimally_informative_prior, iterations = 40000)
pp_check(m8)
summary(m8)
ranef(m8)

m8_a <-
  bayes_test(mortality_bayes_f, former_mortality_prior, iterations = 40000)
pp_check(m8_a)
summary(m8_a)
ranef(m8_a)

write_rds(m1, here("data_clean", "bayesian_models", "testing_current_m1.rds"))
write_rds(m1_a, here("data_clean", "bayesian_models", "testing_current_m1_a.rds"))
write_rds(m2, here("data_clean", "bayesian_models", "testing_former_m2.rds"))
write_rds(m2_a, here("data_clean", "bayesian_models", "testing_former_m2_a.rds"))
write_rds(m3, here("data_clean", "bayesian_models", "hospitalisation_current_m3.rds"))
write_rds(m3_a, here("data_clean", "bayesian_models", "hospitalisation_current_m3_a.rds"))
write_rds(m4, here("data_clean", "bayesian_models", "hospitalisation_former_m4.rds"))
write_rds(m4_a, here("data_clean", "bayesian_models", "hospitalisation_former_m4_a.rds"))
write_rds(m5, here("data_clean", "bayesian_models", "severity_current_m5.rds"))
write_rds(m5_a, here("data_clean", "bayesian_models", "severity_current_m5_a.rds"))
write_rds(m6, here("data_clean", "bayesian_models", "severity_former_m6.rds"))
write_rds(m6_a, here("data_clean", "bayesian_models", "severity_former_m6_a.rds"))
write_rds(m7, here("data_clean", "bayesian_models", "mortality_current_m7.rds"))
write_rds(m7_a, here("data_clean", "bayesian_models", "mortality_current_m7_a.rds"))
write_rds(m8, here("data_clean", "bayesian_models", "mortality_former_m8.rds"))
write_rds(m8_a, here("data_clean", "bayesian_models", "mortality_former_m8_a.rds"))