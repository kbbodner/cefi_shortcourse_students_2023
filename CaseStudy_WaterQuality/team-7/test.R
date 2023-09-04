#test

library(here)
folder <- here("CaseStudy_WaterQuality", "WQ-7")
load(here(folder, "data.RData"))


version$version.string
library(tidybayes)
library(tidyverse)
library(lubridate)
library(rjags)
library(coda)


targets <- read_csv('targets-neon-chla.csv')
target_sites <- targets |> 
  distinct(site_id) |> 
  pull()


aquatic_sites <- read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv") |>
  dplyr::filter(field_site_id %in% target_sites)


glimpse(targets)


targets %>%
  ggplot(., aes(x = datetime, y = observation)) +
  geom_point() +   
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  facet_wrap(~site_id, scales = 'free_y')+
  labs(title = 'chla')

bucket <- "neon4cast-drivers/noaa/gefs-v12/stage3/parquet/"
s3_past <- arrow::s3_bucket(bucket, endpoint_override = "data.ecoforecast.org", anonymous = TRUE)


variables = c("air_temperature", "precipitation_flux")


noaa_past <- arrow::open_dataset(s3_past) |>
  dplyr::filter(site_id %in% target_sites,
                datetime >= ymd('2017-01-01'),
                variable %in% variables) |>
  collect()


noaa_past_mean <- noaa_past |> 
  mutate(datetime = as_date(datetime)) |> 
  group_by(datetime, site_id, variable) |> 
  summarize(prediction = mean(prediction, na.rm = TRUE), .groups = "drop") |> 
  pivot_wider(names_from = variable, values_from = prediction) |> 
  # convert air temp to C
  mutate(air_temperature = air_temperature - 273.15) |> 
  filter(datetime <= as_date(max(targets$datetime)))


# generate forecast(s) for June 2023
forecast_date <- as_date(max(targets$datetime))

bucket <- paste0("neon4cast-drivers/noaa/gefs-v12/stage2/parquet/0/", forecast_date) 
s3_future <- arrow::s3_bucket(bucket = bucket, endpoint_override = 'data.ecoforecast.org', anonymous = TRUE)

variables <- c("air_temperature", "precipitation_flux")

noaa_future <- arrow::open_dataset(s3_future) |> 
  dplyr::filter(datetime >= forecast_date,
                site_id %in% target_sites,
                variable %in% variables) |> 
  collect()

write.csv(noaa_future, file = "noaa_future.csv")

noaa_future_daily <- noaa_future |> 
  mutate(datetime = as_date(datetime)) |> 
  # mean daily forecasts at each site per ensemble
  group_by(datetime, site_id, parameter, variable) |> 
  summarize(prediction = mean(prediction)) |>
  pivot_wider(names_from = variable, values_from = prediction) |>
  # convert to Celsius
  mutate(air_temperature = air_temperature - 273.15) |> 
  select(datetime, site_id, air_temperature, parameter)

noaa_future_daily[1:10,]