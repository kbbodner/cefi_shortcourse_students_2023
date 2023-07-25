# setup -------------------------------------------------------------------
library(tidybayes)
library(tidyverse)
library(lubridate)
library(rjags)

library(here)
folder <- here("CaseStudy_WaterQuality", "WQ-7")
load(here(folder, "data.RData"))

source(here(folder,"toolbox.r"))
# ggthemr::ggthemr("fresh")


# visualize data ----------------------------------------------------------
targets_lm %>%
  ggplot(aes(datetime, chla)) +
  geom_line() +
  facet_wrap(~site_id)


# Bayesian inference ------------------------------------------------------
data_all <- targets_lm %>% 
  pull(site_id) %>% 
  unique() %>% 
  map(function(site_name){
    site_target <- targets_lm |> 
      filter(site_id == site_name)
    
    #Find when the data for the site starts and filter to only 
    #more recent datetimes with no NAs
    first_no_na <- site_target |> 
      filter(!is.na(air_temperature) & !is.na(chla)) |> 
      summarise(min = min(datetime)) |> 
      pull(min)
    
    site_target |> 
      filter(datetime >= first_no_na)
  }) %>% 
  map(
    ~ list(
      air_temp = .$air_temperature,
      y = .$chla,
      n = length(.$air_temperature),
      sd_obs = 0.1,
      chla_init = .$chla[1]
    )
  )

forecast_df <- data_all %>% 
  map(bayesian_inference)