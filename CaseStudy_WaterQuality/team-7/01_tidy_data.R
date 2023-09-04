# setup -------------------------------------------------------------------
library(tidybayes)
library(tidyverse)
library(lubridate)
library(rjags)

library(here)
folder <- here("CaseStudy_WaterQuality", "WQ-7")
# load(here(folder, "data.RData"))
load(here(folder, "data_humid.RData"))

noaa_past <- read_csv(here(folder, "noaa_past.csv"))
noaa_future <- read_csv(here(folder, "noaa_future.csv"))

noaa_past_mean <- noaa_past |> 
  mutate(datetime = as_date(datetime)) |> 
  group_by(datetime, site_id, variable) |> 
  summarize(prediction = mean(prediction, na.rm = TRUE), .groups = "drop") |> 
  pivot_wider(names_from = variable, values_from = prediction) |> 
  # convert air temp to C
  mutate(air_temperature = air_temperature - 273.15) |> 
  filter(datetime <= as_date(max(targets$datetime)))

noaa_past_mean_humid <- noaa_past_humid |> 
  mutate(datetime = as_date(datetime)) |> 
  group_by(datetime, site_id, variable) |> 
  summarize(prediction = mean(prediction, na.rm = TRUE), .groups = "drop") |> 
  pivot_wider(names_from = variable, values_from = prediction)

noaa_future_daily <- noaa_future |> 
  mutate(datetime = as_date(datetime)) |> 
  # mean daily forecasts at each site per ensemble
  group_by(datetime, site_id, parameter, variable) |> 
  summarize(prediction = mean(prediction)) |>
  pivot_wider(names_from = variable, values_from = prediction) |>
  # convert to Celsius
  mutate(air_temperature = air_temperature - 273.15) |> 
  select(datetime, site_id, air_temperature, parameter)

noaa_future_daily_humid <- noaa_future_humid |> 
  mutate(datetime = as_date(datetime)) |> 
  # mean daily forecasts at each site per ensemble
  group_by(datetime, site_id, parameter, variable) |> 
  summarize(prediction = mean(prediction)) |>
  pivot_wider(names_from = variable, values_from = prediction)




targets_lm <- targets |> 
  pivot_wider(names_from = 'variable', values_from = 'observation') |> 
  left_join(noaa_past_mean_humid, 
            by = c("datetime","site_id")) |>
  left_join(noaa_past_mean, 
            by = c("datetime","site_id"))
