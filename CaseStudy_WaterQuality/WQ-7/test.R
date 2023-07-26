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


targets <- read_csv('Data/targets-neon-chla.csv')
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
