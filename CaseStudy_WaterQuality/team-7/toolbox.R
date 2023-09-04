bayesian_inference <- function(data, site_id) {
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
nchain <- 3
inits <- list()
for (i in 1:nchain) {
  inits[[i]] <- list(
    beta1 = rnorm(1, 0.34, 0.05),
    beta2 = rnorm(1, 0.11, 0.05),
    sd_process = runif(1, 0.05, 0.15)
  )
}

j.model <- jags.model(
  file = textConnection(jags_code),
  data = data,
  inits = inits,
  n.chains = 3
)

jags.out <- coda.samples(
  model = j.model,
  variable.names = c(
    "beta1",
    "beta2",
    "chla_latent",
    "sd_process"
  ),
  n.iter = 5000
)

burn_in <- 1000
chain <- jags.out %>%
  tidybayes::spread_draws(beta1, beta2, sd_process, chla_latent[day]) |>
  filter(.iteration > burn_in) |>
  ungroup()

max_day <- max(chain$day)
chain <- chain |>
  filter(day == max_day) |>
  select(-day)


num_samples <- 500

noaa_future_site <- noaa_future_daily |>
  filter(site_id == example_site)

n_days <- length(unique(noaa_future_site$datetime))
chla_latent <- matrix(NA, num_samples, n_days)
y <- matrix(NA, num_samples, n_days) # y is the forecasted observation
forecast_df <- NULL



# loop over posterior samples
for (i in 1:num_samples) {
  sample_index <- sample(x = 1:nrow(chain), size = 1, replace = TRUE)
  noaa_ensemble <- sample(x = 1:30, size = 1)
  noaa_future_site_ens <- noaa_future_site |> filter(parameter == noaa_ensemble)
  # Initialize with a sample from the most recent latent state posterior distribution
  chla_latent[i, 1] <- chain$chla_latent[sample_index]
  y[i, 1] <- rnorm(1, chla_latent[i, 1], sd = data$sd_obs)
  
  # loop over forecast days
  for (j in 2:n_days) {
    pred <- chla_latent[i, j - 1] * chain$beta1[sample_index] +
      noaa_future_site_ens$air_temperature[j] * chain$beta2[sample_index]
    
    chla_latent[i, j] <- rnorm(1, pred, chain$sd_process[sample_index])
    y[i, j] <- rnorm(1, chla_latent[i, j], sd = data$sd_obs)
  }
  
  # Build formatted forecast
  df <- tibble(
    datetime = noaa_future_site_ens$datetime,
    site_id = site_id,
    variable = "chla",
    parameter = i,
    prediction = y[i, ]
  )
  
  forecast_df <- bind_rows(forecast_df, df)
}
forecast_df
}


bayesian_inference_2 <- function(data, site_id) {
  jags_code <- "
model{

  # Priors
  beta1 ~ dnorm(0, 1/10000)
  beta2 ~ dnorm(0, 1/10000)
  beta3 ~ dnorm(0, 1/10000)
  beta4 ~ dnorm(0, 1/10000)
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
      chla_pred[i] <- beta1 * chla_latent[i-1] + beta2 * air_temp[i] +beta3*humid[i]
      +beta4*(air_temp[i])*humid[i]
      chla_latent[i] ~ dnorm(chla_pred[i], tau_process)

      # Data model
      y[i]  ~ dnorm(chla_latent[i], tau_obs)
  }
}
"
nchain <- 3
inits <- list()
for (i in 1:nchain) {
  inits[[i]] <- list(
    beta1 = rnorm(1, 0.34, 0.05),
    beta2 = rnorm(1, 0.11, 0.05),
    beta3 = rnorm(1, 0.25, 0.05),
    beta4 = rnorm(1, 0.25, 0.05),
    sd_process = runif(1, 0.05, 0.15)
  )
}

j.model <- jags.model(
  file = textConnection(jags_code),
  data = data,
  inits = inits,
  n.chains = 3
)

jags.out <- coda.samples(
  model = j.model,
  variable.names = c(
    "beta1",
    "beta2",
    "beta3",
    "beta4",
    "chla_latent",
    "sd_process"
  ),
  n.iter = 5000
)

burn_in <- 1000
chain <- jags.out %>%
  tidybayes::spread_draws(beta1, beta2, beta3, beta4,sd_process, chla_latent[day]) |>
  filter(.iteration > burn_in) |>
  ungroup()

max_day <- max(chain$day)
chain <- chain |>
  filter(day == max_day) |>
  select(-day)


num_samples <- 500

noaa_future_site <- air_humid_future |>
  filter(site_id == example_site)

n_days <- length(unique(noaa_future_site$datetime))
chla_latent <- matrix(NA, num_samples, n_days)
y <- matrix(NA, num_samples, n_days) # y is the forecasted observation
forecast_df <- NULL



# loop over posterior samples
for (i in 1:num_samples) {
  sample_index <- sample(x = 1:nrow(chain), size = 1, replace = TRUE)
  noaa_ensemble <- sample(x = 1:30, size = 1)
  noaa_future_site_ens <- noaa_future_site |> filter(parameter == noaa_ensemble)
  # Initialize with a sample from the most recent latent state posterior distribution
  chla_latent[i, 1] <- chain$chla_latent[sample_index]
  y[i, 1] <- rnorm(1, chla_latent[i, 1], sd = data$sd_obs)
  
  # loop over forecast days
  for (j in 2:n_days) {
    pred <- chla_latent[i, j - 1] * chain$beta1[sample_index] +
      noaa_future_site_ens$air_temperature[j] * chain$beta2[sample_index] +
      noaa_future_site_ens$relative_humidity[j] * chain$beta3[sample_index]+
      noaa_future_site_ens$relative_humidity[j] *
      noaa_future_site_ens$air_temperature[j]*chain$beta4[sample_index]
    
    chla_latent[i, j] <- rnorm(1, pred, chain$sd_process[sample_index])
    y[i, j] <- rnorm(1, chla_latent[i, j], sd = data$sd_obs)
  }
  
  # Build formatted forecast
  df <- tibble(
    datetime = noaa_future_site_ens$datetime,
    site_id = site_id,
    variable = "chla",
    parameter = i,
    prediction = y[i, ]
  )
  
  forecast_df <- bind_rows(forecast_df, df)
}
forecast_df
}



bayesian_inference_3 <- function(data, site_id) {
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
      chla_pred[i] <- beta1 * chla_latent[i-1] + beta2 * humid[i]
      
      chla_latent[i] ~ dnorm(chla_pred[i], tau_process)

      # Data model
      y[i]  ~ dnorm(chla_latent[i], tau_obs)
  }
}
"
nchain <- 3
inits <- list()
for (i in 1:nchain) {
  inits[[i]] <- list(
    beta1 = rnorm(1, 0.34, 0.05),
    beta2 = rnorm(1, 0.11, 0.05),
    sd_process = runif(1, 0.05, 0.15)
  )
}

j.model <- jags.model(
  file = textConnection(jags_code),
  data = data,
  inits = inits,
  n.chains = 3
)

jags.out <- coda.samples(
  model = j.model,
  variable.names = c(
    "beta1",
    "beta2",
    "chla_latent",
    "sd_process"
  ),
  n.iter = 5000
)

burn_in <- 1000
chain <- jags.out %>%
  tidybayes::spread_draws(beta1, beta2, sd_process, chla_latent[day]) |>
  filter(.iteration > burn_in) |>
  ungroup()

max_day <- max(chain$day)
chain <- chain |>
  filter(day == max_day) |>
  select(-day)


num_samples <- 500

noaa_future_site <- noaa_future_daily_humid |>
  filter(site_id == example_site)

n_days <- length(unique(noaa_future_site$datetime))
chla_latent <- matrix(NA, num_samples, n_days)
y <- matrix(NA, num_samples, n_days) # y is the forecasted observation
forecast_df <- NULL



# loop over posterior samples
for (i in 1:num_samples) {
  sample_index <- sample(x = 1:nrow(chain), size = 1, replace = TRUE)
  noaa_ensemble <- sample(x = 1:30, size = 1)
  noaa_future_site_ens <- noaa_future_site |> filter(parameter == noaa_ensemble)
  # Initialize with a sample from the most recent latent state posterior distribution
  chla_latent[i, 1] <- chain$chla_latent[sample_index]
  y[i, 1] <- rnorm(1, chla_latent[i, 1], sd = data$sd_obs)
  
  # loop over forecast days
  for (j in 2:n_days) {
    pred <- chla_latent[i, j - 1] * chain$beta1[sample_index] +
      noaa_future_site_ens$relative_humidity[j] * chain$beta2[sample_index]
    
    chla_latent[i, j] <- rnorm(1, pred, chain$sd_process[sample_index])
    y[i, j] <- rnorm(1, chla_latent[i, j], sd = data$sd_obs)
  }
  
  # Build formatted forecast
  df <- tibble(
    datetime = noaa_future_site_ens$datetime,
    site_id = site_id,
    variable = "chla",
    parameter = i,
    prediction = y[i, ]
  )
  
  forecast_df <- bind_rows(forecast_df, df)
}
forecast_df
}


