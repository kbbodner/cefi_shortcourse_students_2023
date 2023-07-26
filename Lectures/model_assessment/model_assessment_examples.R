library(carData)
data("Wells")
kidiq <- readr::read_csv("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/KidIQ/data/kidiq.csv")
library(tidyverse)
library(brms)

options(future.rng.onMisuse = "ignore")
options(mc.cores = parallel::detectCores())


# transform data
wells_data <- Wells %>% mutate(switch_numeric = if_else(switch == "yes", 1, 0))

# Wells model prior
bprior <- c(
  prior_string("normal(0,10)", coef = "distance", class = "b"),
  prior_string("normal(0,10)", coef = "arsenic", class = "b")
)
fit_prior <- brm(switch_numeric ~ distance + arsenic,
                 data = wells_data,
                 family = bernoulli(), prior = bprior,
                 sample_prior = "only", chains = 2, iter = 1000, cores = 2
)
fit_prior %>% saveRDS(here::here(
  "model_assessment_talk", "rds",
  "first_model_prior.rds"
))


# Wells model posterior
bprior <- c(
  prior_string("normal(0,10)", coef = "distance", class = "b"),
  prior_string("normal(0,10)", coef = "arsenic", class = "b")
)

fit <- brm(switch_numeric ~ distance + arsenic,
           data = wells_data, family = bernoulli(),
           prior = bprior, chains = 2, iter = 1000, cores = 2
)
fit %>% saveRDS(here::here(
  "model_assessment_talk", "rds",
  "first_model_posterior.rds"
))

# linear model prior
fit_kids_prior <- brm(kid_score ~ mom_iq,
                      data = kidiq, family = gaussian(),
                      prior = c(prior_string("normal(0,10")), 
                      sample_prior = "only",
                      chains = 2, iter = 500, cores = 2
)
fit_kids_prior %>% saveRDS(here::here(
  "model_assessment_talk", "rds",
  "kidsiq_prior.rds"
))


# linear model posterior
fit_kids_posterior <- brm(kid_score ~ mom_iq,
                          data = kidiq, family = gaussian(),
                          prior = c(prior_string("normal(0,10")), chains = 2, iter = 500, cores = 2
)
fit_kids_posterior %>% saveRDS(here::here(
  "model_assessment_talk", "rds",
  "kidsiq_posterior.rds"
))
