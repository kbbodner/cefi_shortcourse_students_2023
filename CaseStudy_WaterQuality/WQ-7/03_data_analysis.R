# setup -------------------------------------------------------------------
library(tidybayes)
library(tidyverse)
library(lubridate)
library(rjags)

library(here)
folder <- here("CaseStudy_WaterQuality", "WQ-7")
load(here(folder, "data.RData"))

source(here(folder,"toolbox.r"))
load(here(folder,"bayesian_prediction.rda"))
# ggthemr::ggthemr("fresh")


 
 forecast_df %>% 
   bind_rows() %>% 
   ggplot(aes(datetime, prediction)) +
   geom_line(aes(group = parameter), alpha = .3) + geom_hline(yintercept = 20, color = "red", linetype = "dashed") +
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
   