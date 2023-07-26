## ----setup, include=FALSE-------------------------------------------------------------------------------


## ----eval = F-------------------------------------------------------------------------------------------
## install.packages('remotes')
## install.packages('rjags')
## install.packages('tidybayes')
## install.packages('tidyverse') # collection of R packages for data manipulation, analysis, and visualisation
## install.packages('lubridate') # working with dates and times
## remotes::install_github('eco4cast/neon4cast') # package from NEON4cast challenge organisers to assist with forecast building and submission


## -------------------------------------------------------------------------------------------------------
version$version.string
library(tidybayes)
library(tidyverse)
library(lubridate)
library(rjags)



## ----eval=TRUE, echo = TRUE, error=FALSE, warning=FALSE, message=FALSE----------------------------------
#read in the targets data
targets <- read_csv('CaseStudy_WaterQuality/Data/targets-neon-chla.csv')
target_sites <- targets |> 
  distinct(site_id) |> 
  pull()



## ----eval=TRUE, echo = TRUE, error=FALSE, warning=FALSE, message=FALSE----------------------------------
# read in the sites data
aquatic_sites <- read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv") |>
  dplyr::filter(field_site_id %in% target_sites)


## ----eval = T, echo = TRUE------------------------------------------------------------------------------
glimpse(targets)



## ----eval = TRUE, echo = TRUE, warning=FALSE, fig.dim=c(10,10), fig.cap='Figure: Chlorophyll targets data at two lake sites provided by EFI for the NEON forecasting challgenge.'----

targets %>%
  ggplot(., aes(x = datetime, y = observation)) +
  geom_point() +   
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  facet_wrap(~site_id, scales = 'free_y')+
  labs(title = 'chla')



## ---- message=FALSE-------------------------------------------------------------------------------------
# past stacked weather
bucket <- "neon4cast-drivers/noaa/gefs-v12/stage3/parquet/"

s3_past <- arrow::s3_bucket(bucket, endpoint_override = "data.ecoforecast.org", anonymous = TRUE)

variables <- c("air_temperature", "precipitation_flux")
#Other variable names can be found at https://projects.ecoforecast.org/neon4cast-docs/Shared-Forecast-Drivers.html#stage-3

noaa_past <- arrow::open_dataset(s3_past) |> 
  dplyr::filter(site_id %in% target_sites,
                datetime >= ymd('2017-01-01'),
                variable %in% variables) |> 
  collect()

noaa_past[1:10,]


## -------------------------------------------------------------------------------------------------------
# aggregate the past to mean values
noaa_past_mean <- noaa_past |> 
  mutate(datetime = as_date(datetime)) |> 
  group_by(datetime, site_id, variable) |> 
  summarize(prediction = mean(prediction, na.rm = TRUE), .groups = "drop") |> 
  pivot_wider(names_from = variable, values_from = prediction) |> 
  # convert air temp to C
  mutate(air_temperature = air_temperature - 273.15) |> 
  filter(datetime <= as_date(max(targets$datetime)))



## ---- message=FALSE-------------------------------------------------------------------------------------
# Note: New forecast only available at 5am UTC the next day - so if forecasting in real-time look for yesterday's forecast

# generate forecast(s) for June 2023
forecast_date <- as_date(max(targets$datetime))

bucket <- paste0("neon4cast-drivers/noaa/gefs-v12/stage2/parquet/0/", forecast_date) 
s3_future <- arrow::s3_bucket(bucket = bucket, endpoint_override = 'data.ecoforecast.org', anonymous = TRUE)

noaa_future <- arrow::open_dataset(s3_future) |> 
  dplyr::filter(datetime >= forecast_date,
                site_id %in% target_sites,
                variable %in% variables) |> 
  collect()



## ----warning=F------------------------------------------------------------------------------------------
noaa_future_daily <- noaa_future |> 
  mutate(datetime = as_date(datetime)) |> 
  # mean daily forecasts at each site per ensemble
  group_by(datetime, site_id, parameter, variable) |> 
  summarize(prediction = mean(prediction)) |>
  pivot_wider(names_from = variable, values_from = prediction) |>
  # convert to Celsius
  mutate(air_temperature = air_temperature - 273.15) |> 
  select(datetime, site_id, air_temperature, precipitation_flux, parameter)

noaa_future_daily[1:10,]


## ----echo = T, fig.cap = c('Figure: historic and future NOAA air temeprature forecasts at lake sites', 'Figure: last two months of historic air temperature forecasts and 35 day ahead forecast')----
ggplot(noaa_future_daily, aes(x=datetime, y=air_temperature)) +
  geom_line(aes(group = parameter), alpha = 0.4)+
  geom_line(data = noaa_past_mean, colour = 'darkblue') +
  facet_wrap(~site_id, scales = 'free')

ggplot(noaa_future_daily, aes(x=datetime, y=air_temperature)) +
  geom_line(aes(group = parameter), alpha = 0.4)+
  geom_line(data = noaa_past_mean, colour = 'darkblue') +
  coord_cartesian(xlim = c(forecast_date - days(60),
                           forecast_date + days(35)))+
  facet_wrap(~site_id, scales = 'free')


# precipitations
ggplot(noaa_future_daily, aes(x=datetime, y=precipitation_flux)) +
  geom_line(aes(group = parameter), alpha = 0.4)+
  geom_line(data = noaa_past_mean, colour = 'darkblue') +
  facet_wrap(~site_id, scales = 'free')

ggplot(noaa_future_daily, aes(x=datetime, y=precipitation_flux)) +
  geom_line(aes(group = parameter), alpha = 0.4)+
  geom_line(data = noaa_past_mean, colour = 'darkblue') +
  coord_cartesian(xlim = c(forecast_date - days(60),
                           forecast_date + days(35)))+
  facet_wrap(~site_id, scales = 'free')


## -------------------------------------------------------------------------------------------------------
targets_lm <- targets |> 
  pivot_wider(names_from = 'variable', values_from = 'observation') |> 
  left_join(noaa_past_mean, 
            by = c("datetime","site_id"))

targets_lm[1050:1060,]


## -------------------------------------------------------------------------------------------------------
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


## -------------------------------------------------------------------------------------------------------
jags_code <- "
model{

  # Priors
  beta1 ~ dnorm(0, 1/10000)
  beta2 ~ dnorm(0, 1/10000)
  beta3 ~ dnorm(0, 1/10000)
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
      chla_pred[i] <- beta1 * chla_latent[i-1] + beta2 * air_temp[i] + beta3 * precip[i]
      chla_latent[i] ~ dnorm(chla_pred[i], tau_process)

      # Data model
      y[i]  ~ dnorm(chla_latent[i], tau_obs)
  }
}
"



## -------------------------------------------------------------------------------------------------------
data <- list(air_temp = site_target$air_temperature,
             precip = site_target$precipitation_flux,
             y = site_target$chla,
             n = length(site_target$air_temperature),
             sd_obs = 0.1,
             chla_init = site_target$chla[1])

## -------------------------------------------------------------------------------------------------------
nchain <- 3
inits <- list()
for(i in 1:nchain){
  inits[[i]] <- list(beta1 = rnorm(1, 0.34, 0.05), 
                     beta2 = rnorm(1, 0.11, 0.05),
                     beta3 = rnorm(1, 0.2, 0.05),
                                   
                     sd_process = runif(1, 0.05, 0.15 ))
}


## -------------------------------------------------------------------------------------------------------
j.model <- jags.model(file = textConnection(jags_code),
                      data = data,
                      inits = inits,
                      n.chains = 3)

#Run MCMC
jags.out   <- coda.samples(model = j.model,
                           variable.names = c("beta1", 
                                              "beta2",
                                              "beta3", 
                                              "chla_latent",
                                              "sd_process"),
                           n.iter = 5000)


## ----warning=F------------------------------------------------------------------------------------------

burn_in <- 1000
chain <- jags.out %>%
  tidybayes::spread_draws(beta1, beta2, beta3, sd_process, chla_latent[day]) |> 
  filter(.iteration > burn_in) |> 
  ungroup()

max_day <- max(chain$day)
chain <- chain |> 
  filter(day == max_day) |> 
  select(-day)


## -------------------------------------------------------------------------------------------------------
chain |> 
  pivot_longer(beta1:chla_latent, names_to = "variable" , values_to = "value") |> 
  ggplot(aes(x = .iteration, y = value, color = factor(.chain))) +
  geom_line() +
  facet_wrap(~variable, scale = "free")


## -------------------------------------------------------------------------------------------------------
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
      noaa_future_site_ens$air_temperature[j] * chain$beta2[sample_index] +
      noaa_future_site_ens$precipitation_flux[j] * chain$beta3[sample_index]

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


## ---- echo = T, warning = F-----------------------------------------------------------------------------
ggplot(forecast_df,aes(x=datetime, y=prediction, group = parameter)) + 
  geom_point(data = subset(targets, site_id == 'SUGG'),
             aes(x=datetime, y=observation, group = 'obs'), colour = 'black') +
  geom_line(alpha = 0.3, aes(colour = 'ensemble member (parameter)')) + 
  facet_wrap(~site_id, scales = 'free_y') +
  scale_x_date(expand = c(0,0), date_labels = "%d %b") +
  labs(y = 'value') +
  geom_vline(aes(linetype = 'reference_datetime', 
                 xintercept = as_date(max(targets$datetime))), 
             colour = 'blue', size = 1.5) +
  labs(title = 'site_id', subtitle = 'variable = temperature and precipitation', caption = 'prediction') + 
  annotate("text", x = min(forecast_df$datetime) - days(10), y = 40, label = "past")  +
  annotate("text", x = min(forecast_df$datetime) + days(12), y = 40, label = "future")  +
  theme_bw() +
  coord_cartesian(xlim = c(min(forecast_df$datetime) - 30, Sys.Date())) +
  scale_linetype_manual(values = 'dashed', name = '')+
  scale_colour_manual(name = '', values = 'red')


## -------------------------------------------------------------------------------------------------------
# Remember to change the model_id when you make changes to the model structure!
my_model_id <- 'your_model_id'

forecast_df_efi <- forecast_df %>%
  mutate(model_id = my_model_id,
         reference_datetime = as_date(min(datetime)) - days(1),
         family = 'ensemble',
         parameter = as.character(parameter)) %>%
  select(model_id, datetime, reference_datetime, site_id, family, parameter, variable, prediction)


## ----eval = T-------------------------------------------------------------------------------------------
# Start by writing the forecast to file
theme <- 'aquatics'
date <- forecast_df_efi$reference_datetime[1]
forecast_name_1 <- paste0(forecast_df_efi$model_id[1], ".csv")
forecast_file_1 <- paste(theme, date, forecast_name_1, sep = '-')
forecast_file_1

if (!dir.exists('Forecasts')) {
  dir.create('Forecasts')
}
write_csv(forecast_df_efi, file.path('Forecasts', forecast_file_1))

neon4cast::forecast_output_validator(file.path('Forecasts', forecast_file_1))



## ----eval = FALSE---------------------------------------------------------------------------------------
## # can uses the neon4cast::forecast_output_validator() to check the forecast is in the right format
## neon4cast::submit(forecast_file = file.path('Forecasts', forecast_file_1),
##                   ask = FALSE) # if ask = T (default), it will produce a pop-up box asking if you want to submit

