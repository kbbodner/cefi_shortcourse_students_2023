# purpose: load libraries and packages


## libraries


# mandatory setup for working with macpan and stan
# - - - - - - - - - - - - - - - - - - - - - - - - - - - 
library(McMasterPandemic)
source(here::here("R", "tmbstan_tools.R")) # some custom tools for connecting McMasterPandemic and Stan, they're documented fully in that file
#source("R/tmbstan_tools.R")
setup_stan() # some required set up for using Stan with McMasterPandemic
library(rstan) # for additional R Stan tools

# other tools
# - - - - - - - - - - - - - - - - - - - - - - - - - - - 
library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(
  theme_bw(
    base_size = 16
  )
)


library(here)
library(readr)