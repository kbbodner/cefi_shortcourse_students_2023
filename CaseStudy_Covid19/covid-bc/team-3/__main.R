#' This main file acts to run all of the code for this particular case study
#' in one place as a form of "reproducible" workflow

# set up =======================================================================
library(here)

# run with the stan case study =================================================
source(here("./CaseStudy_Covid19/covid-bc/team-3/R/02_prepare_data.R"))

source(here("./CaseStudy_Covid19/covid-bc/team-3/R/03_negbin_seir.R"))

source(here("./CaseStudy_Covid19/covid-bc/team-3/R/04_mask_seir.R"))