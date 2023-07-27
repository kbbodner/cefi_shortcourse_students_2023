# setup -------------------------------------------------------------------
library(tidybayes)
library(tidyverse)
library(lubridate)
library(rjags)

library(here)
folder <- here("CaseStudy_WaterQuality", "WQ-7")
load(here(folder, "data.RData"))

source(here(folder, "toolbox.r"))


# combine predictions from different methods ------------------------------------------------------------
load(here(folder, "bayesian_prediction.rda"))
forecast_air <- forecast_df %>%
  bind_rows()

load(here(folder, "bayesian_prediction_m2.rda"))
forecast_air_nonlinear <- forecast_df %>%
  bind_rows()

load(here(folder, "bayesian_prediction_humid_air.rda"))
forecast_air_humid <- forecast_df %>%
  bind_rows()

forecast <- bind_rows(
  forecast_air %>%
    mutate(method = "~ Temp"),
  forecast_air %>%
    mutate(method = "~ Temp + Temp^2"),
  forecast_air_humid %>%
    mutate(method = "~ Temp * Humid"),
)

real_data <- read_csv(here(folder, "targets-neon-chla_evaluation.csv"))

forecast <- forecast %>% 
  left_join(real_data) %>% 
  mutate(method = ordered(method, levels = c("~ Temp",
                                             "~ Temp + Temp^2",
                                             "~ Temp * Humid")))

# plot results ------------------------------------------------------------
ggthemr::ggthemr("fresh")

forecast %>%
  ggplot(aes(datetime, prediction, color = site_id)) +
  geom_line(aes(group = interaction(site_id, parameter)), alpha = .05) +
  geom_hline(yintercept = 20, color = "firebrick2", linetype = "dashed") +
  facet_wrap(. ~ method, scales = "free") +
  scale_color_manual(
    values = c("#EFBB24", "#3A8FB7")
  ) +
  labs(
    x = "Date",
    y = "Prediction"
  ) +
  theme(
    legend.title=element_blank(),
    legend.position = 'bottom'
  )
ggsave(here(folder, 'raw_prediction.pdf'), width = 10, height = 5)


forecast %>%
  ggplot(aes(datetime, prediction, color = site_id)) +
  geom_line(aes(group = interaction(site_id, parameter)), alpha = .05) +
  geom_line(aes(y = observation), color = 'black') +
  geom_hline(yintercept = 20, color = "firebrick2", linetype = "dashed") +
  facet_grid(site_id ~ method, scales = "free") +
  scale_color_manual(
    values = c("#EFBB24", "#3A8FB7")
  ) +
  labs(
    x = "Date",
    y = "Prediction"
  ) +
  theme(
    legend.title=element_blank(),
    legend.position = 'none'
  )
ggsave(here(folder, 'compare_prediction_data.pdf'), width = 10, height = 6)


forecast %>%
  group_by(datetime, site_id, method) %>%
  count(swimmable = prediction < 20) %>%
  mutate(prob_swimmable = n / sum(n)) %>%
  filter(swimmable) %>%
  mutate(error = sqrt(prob_swimmable * (prob_swimmable)/ 500)) %>% 
  select(datetime, site_id, prob_swimmable, method, error) %>%
  ggplot(aes(datetime, prob_swimmable,
    group = interaction(site_id, method),
    color = site_id,
    fill = site_id
  )) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = prob_swimmable - error,
                  ymax = ifelse(prob_swimmable + error > 1, 
                                1,
                                prob_swimmable + error)), alpha = .2,
              linetype='dashed') +
  scale_color_manual(
    values = c("#EFBB24", "#3A8FB7")
  ) +
  # scale_fill_manual(
  #   values = c("#EFBB24", "#3A8FB7")
  # ) +
  facet_wrap(~method) +
  labs(
    y = 'Probability of swimmiable',
    x = 'Date'
  ) +
  theme(
    legend.title=element_blank(),
    legend.position = c(.2, .2)
  )
ggsave(here(folder, 'swimmable_prob.pdf'), width = 10, height = 5)


forecast %>%
  group_by(datetime, site_id, method) %>%
  filter(prediction == max(prediction)) %>% 
  ggplot(aes(datetime, prediction,
             group = interaction(site_id, method),
             color = site_id,
             fill = site_id
  )) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c("#EFBB24", "#3A8FB7")
  ) +
  facet_wrap(~method) +
  labs(
    y = 'Max chla predicted',
    x = 'Date'
  ) +
  theme(
    legend.title=element_blank(),
    legend.position = c(.1, .85)
  )
ggsave(here(folder, 'max_prediction.pdf'), width = 10, height = 5)

# compare to past ---------------------------------------------------------
forecast_sub <- forecast %>%
  mutate(year = year(datetime)) %>% 
  mutate(date = format(datetime, "%m-%d")) %>% 
  filter(method == "~ Temp * Humid") %>% 
  rename(chla = prediction) %>% 
  select(-method, -observation)

past_sub <- targets_lm %>% 
  mutate(year = year(datetime)) %>% 
  mutate(date = format(datetime, "%m-%d")) %>% 
  filter(date %in% forecast_sub$date) %>% 
  select(-air_temperature)

ggthemr::ggthemr('fresh', 'clean')
mean_sd <- function(x, mult = 1) {  
  x <- na.omit(x)
  sd <- sd(x)
  mean <- mean(x)
  data.frame(y = mean, ymin = mean - sd, ymax = mean + sd)
}
past_sub %>%
  filter(chla < 100) %>% 
  filter(year != 2023) %>% 
  ggplot(aes(date, chla, color = year, group = year)) +
  geom_line() +
  facet_wrap(~site_id) +
  # geom_smooth(data = forecast_sub, aes(x = date, y = chla),
  #             color = 'black') +
  stat_summary(data = forecast_sub, aes(x = date, y = chla),
               fun.data=mean_sd, geom="ribbon", alpha=0.25, 
               color = 'purple',
               linetype = 'dashed') +
  viridis::scale_color_viridis() +
  geom_hline(yintercept = 20, color = "firebrick2", linetype = "dashed") +
  labs(
    x = "Date",
    y = "Prediction"
  ) +
  theme(
    legend.title=element_blank(),
    legend.position = c(.4, .8)
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave(here(folder, 'compare2other_years.pdf'), width = 10, height = 5)
