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
site_all <- targets_lm %>% 
  pull(site_id) %>% 
  unique()
data_all <- site_all %>% 
  map(function(site_name){
    site_target <- targets_lm |> 
      filter(site_id == site_name)
    
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

forecast_df <- 1:2 %>% 
  map(~bayesian_inference(data_all[[.]], site_all[[.]]))

saveRDS(forecast_df, 
        file = here(folder, 'bayesian_prediction.rda'))

forecast_df %>% 
  bind_rows() %>% 
  ggplot(aes(datetime, prediction)) +
  geom_line(aes(group = parameter), alpha = .3) +
  facet_wrap(~site_id)

forecast_df %>% 
  bind_rows() %>% 
  ggplot(aes(datetime, prediction, color = site_id)) +
  geom_smooth()

forecast_df %>% 
  bind_rows() %>% 
  ggplot(aes(datetime, prediction, color = site_id)) +
  geom_line(aes(group = interaction(parameter,site_id), 
            alpha = .1))

forecast_df %>% 
  bind_rows() %>% 
  group_by(datetime, site_id) %>% 
  count(swimmable = prediction < 20) %>% 
  mutate(prob_swimmable = n/sum(n)) %>% 
  filter(swimmable) %>% 
  select(datetime, site_id, prob_swimmable) %>% 
  ggplot(aes(datetime, prob_swimmable, 
             group = site_id,
             color = site_id)) +
  geom_line() 
  
