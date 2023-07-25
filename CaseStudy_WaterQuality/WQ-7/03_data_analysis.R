# setup -------------------------------------------------------------------
library(tidybayes)
library(tidyverse)
library(lubridate)
library(rjags)

library(here)
folder <- here("CaseStudy_WaterQuality", "WQ-7")
load(here(folder, "data.RData"))

source(here(folder,"toolbox.r"))
load(here(folder,"bayesian_prediction.rda"))
# ggthemr::ggthemr("fresh")
