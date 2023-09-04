num_samples <- 500

head(noaa_past_mean)

head(noaa_future_daily)





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
            aes(x = datetime, y = value, group = var),
            linetype = 2, color = 'red', alpha = .9) +
  geom_hline(yintercept = 20, color = 'darkred') +
  geom_point(data = evo_dt %>% filter(site_id == 'SUGG',
                                      datetime <= '2023-07-05') %>% mutate(datetime = as.Date(datetime)) %>% 
               filter(datetime <= '2023-07-05'),
             aes(x = datetime, y = observation, group = 1), pch = 21, fill = 'red') 



head(var.mat[ 1:5])


library(ggplot2)
library(ggside)

ggplot(data = var.mat[, 1:5],
       aes(x = beta1, y =beta3)) +
  geom_density_2d() +
  geom_xsidehistogram(bins = 20) +
  geom_ysidehistogram(bins = 20) + theme_bw()


var.mat[, 1:3] %>% 
  mutate(id = seq(nrow(var.mat))) %>% 
  pivot_longer(cols =-id,
               names_to = 'var',
               values_to = 'value') %>% 
  ggplot(aes(x = value)) +
  geom_histogram(color = 'gray90', fill = 'lightblue') +
  facet_grid(~var,
             scale = 'free_x') + theme_bw() +
  geom_vline(data = var.mat[, 1:3] %>% 
               mutate(id = seq(nrow(var.mat))) %>% 
               pivot_longer(cols =-id,
                            names_to = 'var',
                            values_to = 'value') %>% group_by(var) %>% summarise(avg = mean(value)),
             aes(xintercept = avg), color = 'red')
