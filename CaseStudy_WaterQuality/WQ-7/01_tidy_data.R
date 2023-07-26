# setup -------------------------------------------------------------------
library(tidybayes)
library(tidyverse)
library(lubridate)
library(rjags)

library(here)
folder <- here("CaseStudy_WaterQuality", "WQ-7")
# load(here(folder, "data.RData"))
load(here(folder, "data_with_precipitation.RData"))

noaa_past <- read_csv(here(folder, "noaa_past.csv"))
noaa_future <- read_csv(here(folder, "noaa_future.csv"))

noaa_future_daily <- noaa_future |> 
  mutate(datetime = as_date(datetime)) |> 
  # mean daily forecasts at each site per ensemble
  group_by(datetime, site_id, parameter, variable) |> 
  summarize(prediction = mean(prediction)) |>
  pivot_wider(names_from = variable, values_from = prediction) |>
  # convert to Celsius
  mutate(air_temperature = air_temperature - 273.15) |> 
  select(datetime, site_id, air_temperature, parameter)

targets_lm <- targets |> 
  pivot_wider(names_from = 'variable', values_from = 'observation') |> 
  left_join(noaa_past_mean, 
            by = c("datetime","site_id"))
