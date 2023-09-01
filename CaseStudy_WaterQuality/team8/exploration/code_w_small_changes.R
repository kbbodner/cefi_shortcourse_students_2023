library(tidybayes)
library(tidyverse)
library(lubridate)
library(rjags)


targets <- read_csv('Data/targets-neon-chla.csv')
target_sites <- targets |> 
  distinct(site_id) |> 
  pull()

aquatic_sites <- read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv") |>
  dplyr::filter(field_site_id %in% target_sites)


targets %>%
  ggplot(., aes(x = datetime, y = observation)) +
  geom_point() +   
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  facet_wrap(~site_id, scales = 'free_y')+
  labs(title = 'chla')

bucket <- "neon4cast-drivers/noaa/gefs-v12/stage3/parquet/"
s3_past <- arrow::s3_bucket(bucket, endpoint_override = "data.ecoforecast.org", anonymous = TRUE)

# variables <- c("air_temperature")
#Other variable names can be found at https://projects.ecoforecast.org/neon4cast-docs/Shared-Forecast-Drivers.html#stage-3

noaa_past <- arrow::open_dataset(s3_past) |> 
  dplyr::filter(site_id %in% target_sites,
                datetime >= ymd('2017-01-01'),
                ) |> 
  collect()

noaa_past[1:10,]



noaa_daily_dt <- noaa_past %>% select(datetime:latitude, variable, prediction) %>% 
  mutate(date = as_date(datetime)) %>% 
  group_by(date, site_id, variable) %>% 
  summarise(min_pred = min(prediction, na.rm = T),
            max_pred = max(prediction, na.rm = T),
            avg_pred = mean(prediction, na.rm = T)) %>% 
  pivot_longer(cols = min_pred:avg_pred,
               names_to = 'var',
               values_to = 'pred') %>% 
  mutate(var = str_replace(var, '_pred', ''),
         var2 = paste0(variable, '_', var))


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

variables <- c("air_temperature")

noaa_future <- arrow::open_dataset(s3_future) |> 
  dplyr::filter(datetime >= forecast_date,
                site_id %in% target_sites,
                variable %in% variables) |> 
  collect()



## y_i ~ y_(i-1) + air_temp(i)
jags_code <- "
model{

  # Priors
  beta1 ~ dnorm(0, 1/10000)
  beta2 ~ dnorm(0, 1/10000)
  sd_process ~ dunif(0.00001, 100)
  
  #Convert Standard Deviation to precision
  tau_obs <- 1 / pow(sd_obs, 2)
  tau_process <- 1 / pow(sd_process, 2)

  #Initial conditions
  chla_latent[1] <- chla_init
  y[1] ~ dnorm(chla_latent[1], tau_process)  

  #Loop through data points
  for(i in 2:n){
      # Process model
      chla_pred[i] <- beta1 * chla_latent[i-1] + beta2 * air_temp[i]
      chla_latent[i] ~ dnorm(chla_pred[i], tau_process)

      # Data model
      y[i]  ~ dnorm(chla_latent[i], tau_obs)
  }
}
"



# 
# ## y_i ~ y_(i-3) + air_temp(i-3)**2+ air_temp(i-2) + air_temp(i-1)
# jags_code <- "
# model{
# 
#   # Priors
#   beta1 ~ dnorm(0, 1/10000)
#   beta2 ~ dnorm(0, 1/10000)
#   beta3 ~ dnorm(0, 1/10000)
#   beta4 ~ dnorm(0, 1/10000)
#   sd_process ~ dunif(0.00001, 100)
#   
#   #Convert Standard Deviation to precision
#   tau_obs <- 1 / pow(sd_obs, 2)
#   tau_process <- 1 / pow(sd_process, 2)
# 
#   #Initial conditions
#   chla_latent[1] <- chla_init
#   chla_latent[2] <- chla_init
#   chla_latent[3] <- chla_init
#   y[1] ~ dnorm(chla_latent[1], tau_process)  
# 
#   #Loop through data points
#   for(i in 4:n){
#       # Process model
#       chla_pred[i] <- beta1 * chla_latent[(i-3)] + beta2 * air_temp[(i-3)]^2 +
#       beta3 * air_temp[(i-2)] + beta4*air_temp[(i-1)]
#       chla_latent[i] ~ dnorm(chla_pred[i], tau_process)
# 
#       # Data model
#       y[i]  ~ dnorm(chla_latent[i], tau_obs)
#   }
# }
# "
# 
# ## y_i ~ y_(i-3) + [air_temp(i-3)+ air_temp(i-2)**2 + air_temp(i-1)]
# jags_code <- "
# model{
# 
#   # Priors
#   beta1 ~ dnorm(0, 1/10000)
#   beta2 ~ dnorm(0, 1/10000)
#   beta3 ~ dnorm(0, 1/10000)
#   beta4 ~ dnorm(0, 1/10000)
#   sd_process ~ dunif(0.00001, 100)
#   
#   #Convert Standard Deviation to precision
#   tau_obs <- 1 / pow(sd_obs, 2)
#   tau_process <- 1 / pow(sd_process, 2)
# 
#   #Initial conditions
#   chla_latent[1] <- chla_init
#   chla_latent[2] <- chla_init
#   chla_latent[3] <- chla_init
#   y[1] ~ dnorm(chla_latent[1], tau_process)  
# 
#   #Loop through data points
#   for(i in 4:n){
#       # Process model
#       chla_pred[i] <- beta1 * chla_latent[(i-3)] + beta2 * air_temp[(i-3)]+
#       beta3 * (air_temp[(i-2)])^2 + beta4*air_temp[(i-1)]
#       chla_latent[i] ~ dnorm(chla_pred[i], tau_process)
# 
#       # Data model
#       y[i]  ~ dnorm(chla_latent[i], tau_obs)
#   }
# }
# "


# 
# ## y_i ~ y_(i-3) + air_temp(i-3)+ air_temp(i-2) + air_temp(i-1)
# jags_code <- "
# model{
# 
#   # Priors
#   beta1 ~ dnorm(0, 1/10000)
#   beta2 ~ dnorm(0, 1/10000)
#   beta3 ~ dnorm(0, 1/10000)
#   beta4 ~ dnorm(0, 1/10000)
#   sd_process ~ dunif(0.00001, 100)
#   
#   #Convert Standard Deviation to precision
#   tau_obs <- 1 / pow(sd_obs, 2)
#   tau_process <- 1 / pow(sd_process, 2)
# 
#   #Initial conditions
#   chla_latent[1] <- chla_init
#   chla_latent[2] <- chla_init
#   chla_latent[3] <- chla_init
#   y[1] ~ dnorm(chla_latent[1], tau_process)  
# 
#   #Loop through data points
#   for(i in 4:n){
#       # Process model
#       chla_pred[i] <- beta1 * chla_latent[(i-3)] + beta2 * air_temp[(i-3)]+
#       beta3 * (air_temp[(i-2)]) + beta4*air_temp[(i-1)]
#       chla_latent[i] ~ dnorm(chla_pred[i], tau_process)
# 
#       # Data model
#       y[i]  ~ dnorm(chla_latent[i], tau_obs)
#   }
# }
# "
# 



# ## y_i ~ y_(i-3)*[air_temp(i-3)+ air_temp(i-2) + air_temp(i-1)]
# jags_code <- "
# model{
# 
#   # Priors
#   beta1 ~ dnorm(0, 1/10000)
#   beta2 ~ dnorm(0, 1/10000)
#   # beta3 ~ dnorm(0, 1/10000)
#   # beta4 ~ dnorm(0, 1/10000)
#   sd_process ~ dunif(0.00001, 100)
#   
#   #Convert Standard Deviation to precision
#   tau_obs <- 1 / pow(sd_obs, 2)
#   tau_process <- 1 / pow(sd_process, 2)
# 
#   #Initial conditions
#   chla_latent[1] <- chla_init
#   chla_latent[2] <- chla_init
#   chla_latent[3] <- chla_init
#   y[1] ~ dnorm(chla_latent[1], tau_process)  
# 
#   #Loop through data points
#   for(i in 4:n){
#       # Process model
#       chla_pred[i] <- beta1 + beta2 * chla_latent[(i-3)] * (air_temp[(i-3)]+
#       air_temp[(i-2)] + air_temp[(i-1)])
#       chla_latent[i] ~ dnorm(chla_pred[i], tau_process)
# 
#       # Data model
#       y[i]  ~ dnorm(chla_latent[i], tau_obs)
#   }
# }
# "







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

targets_lm[1050:1060,]


example_site <- 'SUGG'


site_target <- targets_lm |> 
  filter(site_id == example_site)

#Find when the data for the site starts and filter to only 
#more recent datetimes with no NAs
first_no_na <- site_target |> 
  filter(!is.na(air_temperature) & !is.na(chla)) |> 
  summarise(min = min(datetime)) |> 
  pull(min)

site_target <- site_target |> 
  filter(datetime >= first_no_na)


data <- list(air_temp = site_target$air_temperature,
             y = site_target$chla,
             n = length(site_target$air_temperature),
             sd_obs = 0.1,
             chla_init = mean(site_target$chla, na.rm = T))

nchain <- 3
inits <- list()
for(i in 1:nchain){
  inits[[i]] <- list(beta1 = rnorm(1, 0.34, 0.05), 
                     beta2 = rnorm(1, 0.11, 0.2),
                     # beta3 = rnorm(1, 0.2, 0.2),
                     # beta4 = rnorm(1, 0.1, 0.2),
                     sd_process = runif(1, 0.05, 0.15 ))
}


j.model <- jags.model(file = textConnection(jags_code),
                      data = data,
                      inits = inits,
                      n.chains = 3)


jags.out   <- coda.samples(model = j.model,
                           variable.names = c("beta1", 
                                              "beta2",
                                              'beta3',
                                              'beta4',
                                              "chla_latent",
                                              "sd_process"),
                           n.iter = 50000)

burn_in <- 1000
chain <- jags.out %>%
  tidybayes::spread_draws(beta1, beta2, 
                          # beta3, beta4, 
                          sd_process, chla_latent[day]) |> 
  filter(.iteration > burn_in) |> 
  ungroup()

max_day <- max(chain$day)
chain <- chain |> 
  filter(day == max_day) |> 
  select(-day)

chain |> 
  pivot_longer(beta1:chla_latent, names_to = "variable" , values_to = "value") |> 
  ggplot(aes(x = .iteration, y = value, color = factor(.chain))) +
  geom_line() +
  facet_wrap(~variable, scale = "free")




num_samples <- 500

noaa_future_site <- noaa_future_daily |> 
  filter(site_id == example_site)

n_days <- length(unique(noaa_future_site$datetime))
chla_latent <- matrix(NA, num_samples, n_days)   
y <- matrix(NA, num_samples, n_days) #y is the forecasted observation
forecast_df <- NULL



#loop over posterior samples
for(i in 1:num_samples){
  sample_index <- sample(x = 1:nrow(chain), size = 1, replace = TRUE)
  noaa_ensemble <- sample(x = 1:30, size = 1)
  noaa_future_site_ens <- noaa_future_site |> filter(parameter == noaa_ensemble)
  #Initialize with a sample from the most recent latent state posterior distribution
  chla_latent[i,1] <- chain$chla_latent[sample_index]
  y[i, 1] <- rnorm(1, chla_latent[i,1], sd = data$sd_obs)
  
  #loop over forecast days
  for(j in 2:n_days){
    pred <- chla_latent[i,j-1] * chain$beta1[sample_index] +
      noaa_future_site_ens$air_temperature[j] * chain$beta2[sample_index]
    
    chla_latent[i,j] <- rnorm(1, pred, chain$sd_process[sample_index])
    y[i,j] <- rnorm(1, chla_latent[i,j], sd = data$sd_obs)
    
  }
  
  #Build formatted forecast
  df <- tibble(datetime = noaa_future_site_ens$datetime,
               site_id = example_site,
               variable = "chla",
               parameter = i,
               prediction = y[i, ])
  
  forecast_df <- bind_rows(forecast_df, df)
}


head(forecast_df)
dim(forecast_df)

forecast_df %>% group_by(datetime, site_id) %>% 
  summarise(q50 = median(prediction),
            q25 = quantile(prediction, .25),
            q75 = quantile(prediction, .75)) %>% 
  pivot_longer(cols = q50:q75,
               names_to = 'var',
               values_to = 'value')



evo_dt <- read.csv('targets-neon-chla_evaluation.csv')
head(evo_dt)





forecast_df %>% 
  ggplot(.,aes(x=datetime, y=prediction, group = parameter)) + 
  geom_point(data = subset(targets, site_id == 'SUGG'),
             aes(x=datetime, y=observation, group = 'obs'), colour = 'black') +
  # geom_line(alpha = 0.1, aes(colour = 'ensemble member (parameter)')) + 
  facet_wrap(~site_id, scales = 'free_y') +
  scale_x_date(expand = c(0,0), date_labels = "%d %b") +
  labs(y = 'value') +
  geom_vline(aes(#linetype = 'reference_datetime', 
                 xintercept = as_date(max(targets$datetime))), 
             colour = 'blue', size = 1.5) +
  labs(title = 'site_id', subtitle = 'variable = temperature', caption = 'prediction') + 
  annotate("text", x = min(forecast_df$datetime) - days(10), y = 40, label = "past")  +
  annotate("text", x = min(forecast_df$datetime) + days(12), y = 40, label = "future")  +
  theme_bw() +
  coord_cartesian(xlim = c(min(forecast_df$datetime) - 30, Sys.Date()-15)) +
  scale_linetype_manual(values = 'dashed', name = '')+
  # scale_colour_manual(name = '', values = 'red') +
  geom_line(data = forecast_df %>% group_by(datetime, site_id) %>% 
              summarise(q50 = median(prediction),
                        q25 = quantile(prediction, .25),
                        q75 = quantile(prediction, .75)) %>% 
              pivot_longer(cols = q50:q75,
                           names_to = 'var',
                           values_to = 'value'),
            aes(x = datetime, y = value, group = var, color = 'base_model'),
            linetype = 2) +
  geom_hline(yintercept = 20, color = 'darkred') +
  geom_point(data = evo_dt %>% filter(site_id == 'SUGG',
                                      datetime <= '2023-07-05') %>% mutate(datetime = as.Date(datetime)) %>% 
                                                                             filter(datetime <= '2023-07-05'),
             aes(x = datetime, y = observation, group = 1), pch = 21, fill = 'red') 


