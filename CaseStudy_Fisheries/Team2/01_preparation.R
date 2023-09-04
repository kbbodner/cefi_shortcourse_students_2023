# Loads required libraries
# Loads Functions.R
# Loads data as data.raw
# @data dataframe for specific population

###Load libraries-------------
library(rjags)
library(coda)
library(tidyverse)
library(gsl) # need for lambert's W
library(R2jags)
library(data.table)
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


###Load environmental data-----------
data.env <- read_csv("../DataIn/FC_Environmental_Data.csv")

#Join to stock data and remove real year

data <- data |> 
  left_join( data.env, by = "yr") |> 
  select(-c(Real_Year)) 


###Plot timeseries and state of population
ricker.crude <- GetRickerBMs(data)

ggplot(data) +
  geom_line(aes(x=yr, y=S)) +
  geom_hline(aes(yintercept=ricker.crude$SMSY), color="green", size=1.5 ) +
  geom_hline(aes(yintercept=ricker.crude$Sgen), color="red", size=1.5 )



