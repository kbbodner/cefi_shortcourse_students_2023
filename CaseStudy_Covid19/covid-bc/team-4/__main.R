#' This main file acts to run all of the code for this particular case study
#' in one place as a form of "reproducible" workflow

# set up =======================================================================
library(here)

# run with the stan case study =================================================
source(here(
  "./CaseStudy_Covid19/covid-bc/team-4/R/introduction_stan_seir_case_new_model_res.Qmd"))
