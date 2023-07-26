# preparation, loading data, functions
###Load libraries-------------
library(rjags)
library(coda)
library(tidyverse)
library(gsl) # need for lambert's W
library(R2jags)
library("rstudioapi")


###Load functions-------------
setwd(dirname(getActiveDocumentContext()$path))
source("../Code/Functions.R")


###Load data-------------
data.raw <- read_csv("../DataIn/FraserSockeyeData2022.csv")

#Filter on population
selected.pop <- "Early Stuart"

data <- data.raw |>
  filter(Pop_Name == selected.pop)