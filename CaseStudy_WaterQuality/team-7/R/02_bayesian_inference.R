# setup -------------------------------------------------------------------
library(tidybayes)
library(tidyverse)
library(lubridate)
library(rjags)

library(here)
folder <- here("CaseStudy_WaterQuality", "team-7", "data-outputs")
load(here(folder, "data_with_precipitation.RData"))

#source(here(folder,"toolbox.r"))
# ggthemr::ggthemr("fresh")


# visualize data ----------------------------------------------------------
targets_lm %>%
  ggplot(aes(datetime, chla)) +
  geom_line() +
  facet_wrap(~site_id)


# Bayesian inference ------------------------------------------------------
site_all <- targets_lm %>% 
  pull(site_id) %>% 
  unique()
data_all <- site_all %>% 
  map(function(site_name){
    site_target <- targets_lm |> 
      filter(site_id == site_name)
    
    first_no_na <- site_target |> 
      filter(!is.na(air_temperature) & !is.na(chla) & !is.na(relative_humidity)) |> 
      summarise(min = min(datetime)) |> 
      pull(min)
    
    site_target |> 
      filter(datetime >= first_no_na)
  }) %>% 
  map(
    ~ list(
      air_temp = .$air_temperature,
      humid= .$relative_humidity,
      y = .$chla,
      n = length(.$relative_humidity),
      sd_obs = 0.1,
      chla_init = .$chla[1]
    )
  )

forecast_df <- 1:2 %>% 
  map(~bayesian_inference_2(data_all[[.]], site_all[[.]]))

save(forecast_df, 
     file = here(folder, 'bayesian_prediction_humid_air.rda'))

