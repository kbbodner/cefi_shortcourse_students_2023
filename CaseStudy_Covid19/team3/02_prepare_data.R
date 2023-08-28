## Packages

options(future.rng.onMisuse = "ignore")
options(mc.cores = parallel::detectCores())

# Set-up Stan and related libraries, as well as libraries for data and
# data visualization.

# Bayesian computation
library(rstan)
library(tidybayes)

# data science packages
library(dplyr)
library(magrittr)
library(ggplot2)
library(readr)
library(here)
ymd <- lubridate::ymd

## Data

# First, we will read in the British Columbia, Canada COVID-19
# reported-case data.

## Load the BC COVID-19 data

# load data grouped by location (HA), and age group
covid_data <- read_csv(here("data","fitting_data.csv"))


covid_data %>%
  group_by(Reported_Date, HA) %>%
  tally() %>%
  ggplot(aes(x = Reported_Date, y = n, color = HA)) +
  geom_line() +
  theme_classic()

# Load Total BC cases for whole province for first 60 recorded time-points
bc_data <- read_csv(here("data","bc_fitting_data.csv"))

bc_data %>%
  ggplot(aes(x = Reported_Date, y = n)) +
  geom_line() +
  theme_classic() +
  ggtitle("Reported cases BC (first 60 recorded time-points of pandemic)")

## Prepare forecasting data

# The task will be to use the first 30 recorded time-points of data to forecast
# the next 30 days of cases.
# In order to do this we first need to define over what days to forecast for. As
# stan code can't handle dates in as clean a way that can be done in R, we will
# instead generate the days to forecast as the number of days since the start of
# the data

# We then store the time points and cases data to load into the stan model

fitting_time_points <- bc_data$time_points
fitting_cases <- bc_data$n
forecast_time_points <- seq(max(fitting_time_points) + 1,
                            max(fitting_time_points) + 31)

forecast_dates <- seq(max(bc_data$Reported_Date) + 1,
                      max(bc_data$Reported_Date) + 31, by = "1 day")
