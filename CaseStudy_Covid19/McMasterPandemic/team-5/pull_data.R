#load packages
library(here)
library(readr)
library(ggplot2)
library(dplyr)

#----------------------------------------------------------------------------------------------
#load data
# COVID-19 case data for Ontario dates pre 1 April 2020
cases = read_csv(here("data", "fitting.csv")) 

#----------------------------------------------------------------------------------------------
#prep simulation data - this is for modelling, but good to look at/know


# initial state, in units of individuals
state = c(
  S = 2000, #S = susceptible
  E = 0,  #E = exposed,
  I = 1,  #I = infected, first date of infection/known exposure is 2020-02-08
  R = 0 #R = recovered
)

# parameters
params = c(
  beta = 0.7,
  N = sum(state),
  alpha = 0.05,
  gamma = 0.06
)

#----------------------------------------------------------------------------------------------
