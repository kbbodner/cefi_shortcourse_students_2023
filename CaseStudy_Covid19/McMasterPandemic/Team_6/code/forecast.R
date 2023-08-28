# mandatory setup for working with macpan and stan
# - - - - - - - - - - - - - - - - - - - - - - - - - - - 
library(McMasterPandemic)
source(here::here("R", "tmbstan_tools.R")) # some custom tools for connecting McMasterPandemic and Stan, they're documented fully in that file
setup_stan() # some required set up for using Stan with McMasterPandemic
library(rstan) # for additional R Stan tools

# other tools
# - - - - - - - - - - - - - - - - - - - - - - - - - - - 
library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(
  theme_bw(
    base_size = 16
  )
)

# initial state, in units of individuals
state = c(
  S = 15000000, 
  E = 0,  
  I = 1, 
  R = 0,
  H = 0
  #  D = 0
)

# parameters
params = c(
  beta = 0.7,
  N = sum(state),
  alpha = 0.2,
  lambda = 0.018, #self-calculated for March31st (see first slide)
  #  theta = 0.1, #The first wave CFR ranged from 0.004 to 0.146
  gamma = 0.1,
  sigma = 1/21 # Stay at hospital
)

param_schedule = data.frame(
  Date = as.Date('2020-03-25'),
  Symbol = 'beta',
  Value = NA,
  Type = 'rel_prev' #relative
)

# start and end dates specified as strings,
# they don't have to be Date types, but they can be
start_date = "2020-02-07"
end_date = "2020-03-31"

# base model
model = (flexmodel(
  params = params,
  state = state,
  start_date = start_date,
  end_date = end_date,
  do_hazard = TRUE # an option to ensure computations are stable when any of the state variables approach zero (keep this switched on!)
)
  %>% add_rate(
    "S", # from compartment S
    "E", # to compartment I
    ~ (beta) * (1/N) * (I) # expression for the flow in terms of parameters and state variables, as a formula
  )
  %>% add_rate("E", "I", ~ (alpha)*(1 - lambda))
  %>% add_rate("I", "R", ~ (gamma))     
  %>% add_rate("E", "H", ~ (alpha)*(lambda)) #hospitalization
  %>% add_rate("H", "R", ~ (sigma)) #hospitalization
  # %>% add_rate("H", "D", ~ (theta)) # deaths
  %>% update_error_dist(
    S ~ poisson(),
    E ~ poisson(),
    I ~ poisson(),
    R ~ poisson(),
    #D ~ poisson(),
    H ~ poisson()
  )
  # add an expression to calculate in the simulation report
  %>% add_sim_report_expr("incidence", ~ (S_to_E) * (S))
  # add_error_dist instead of update_ because 
  # update_ replaces previously attached error distributions
  # add_ appends
  %>% add_error_dist( 
    incidence ~ poisson() #change this error
  )
  # time-varying beta
  %>% update_piece_wise(param_schedule)
)

# observed data
observed <- read.csv("data/fitting.csv") %>%
  mutate(
    date = as.Date(date), # convert to date
    var = 'incidence' # add a variable for the name of sim output
  ) %>%
  select(date, var, value = new_cases)

# model to calibrate
model_to_calibrate = (model
  # attach observed data
  %>% update_observed(
    observed
  )
  # attach priors for parameters we're fitting
  # ("optimizing" over)
  %>% update_opt_params(
    # fitting log beta
    log_beta ~ log_normal(
      -1, # log mean, so mean beta is exp(-1) = 0.36  these are the parameters for the log distribution which result on different X, see presentation
      0.5 # standard deviation of the log normal prior 
    )
  )
  # attach prior for beta change on 2023-04-01
  %>% add_opt_tv_params(
    tv_type = "rel_prev", # type of time-variation value, in this case absolute
    log_beta ~ log_normal(-1, 0.5)
  )
)

model_fit = calibrate_stan(
  model = model, # original model object
  model_to_calibrate = model_to_calibrate, # model object with observed data and priors
  chains = 2 # number of MCMC chains
)

(model_fit
  %>% ensemble_stan(n_cores = 4) # generate ensemble in parallel
  %>% summarise_ensemble_stan()
)

rstan::traceplot(model_fit$fit)

validation <- read.csv("data/validation.csv") %>%
  mutate(
    date = as.Date(date), # convert to date
    var = 'incidence' # add a variable for the name of sim output
  ) %>%
  select(date, var, value = new_cases)

# make a function because we'll reuse this in this doc
plot_ensemble <- function(ens, obs){
  value_type_labels <- c("Model fit", "Observed data")
  
  df = (ens
        %>% filter(var == "incidence")
        %>% mutate(value_type = value_type_labels[1])
        %>% bind_rows(
          obs %>% mutate(value_type = value_type_labels[2])
        )
  )
  
  colour_palette = c("dodgerblue", "black")
  names(colour_palette) = value_type_labels
  
  (ggplot(df, aes(x = date))
    # observed points
    + geom_point(
      data = df %>% filter(value_type == value_type_labels[2]),
      mapping = aes(y = value, colour = value_type), 
      shape = 1, size = 2
    )
    # simulation from fitted model
    + geom_ribbon(
      data = df %>% filter(value_type == value_type_labels[1]),
      mapping = aes(ymin = lwr, ymax = upr,
                    fill = value_type), alpha = 0.3
    )
    + geom_line(
      data = df %>% filter(value_type == value_type_labels[1]),
      mapping = aes(y = value, colour = value_type),
      linewidth = 1.25
    )
    + scale_colour_manual(values = colour_palette,
                          limits = value_type_labels)
    + scale_fill_manual(values = colour_palette,
                        limits = value_type_labels)
    + labs(title = "Incidence over time")
    + guides(
      color =
        guide_legend(override.aes = list(
          shape = c(NA, 1),
          linewidth = c(1.25, NA),
          fill = c(colour_palette[[1]], NA))
        )
    )
    + theme(
      axis.title = element_blank(),
      legend.title = element_blank(),
      legend.justification = c(0,1),
      legend.position = c(0,1),
      legend.background = element_rect(fill = NA),
      legend.key = element_rect(fill = NA)
    )
  )
}

plot_ensemble_val <- function(ens, obs, val){
  value_type_labels <- c("Model fit", "Observed data","Validation data")
  df = (ens
        %>% filter(var == "incidence")
        %>% mutate(value_type = value_type_labels[1])
        %>% bind_rows(
          obs %>% mutate(value_type = value_type_labels[2])
        )
        %>% bind_rows(
          val %>% mutate(value_type = value_type_labels[3])
        )
  )
  
  colour_palette = c("dodgerblue", "black","red")
  names(colour_palette) = value_type_labels
  
  (ggplot(df, aes(x = date))
    # observed points
    + geom_point(
      data = df %>% filter(value_type == value_type_labels[2]),
      mapping = aes(y = value, colour = value_type), 
      shape = 1, size = 2
    )
    # observed points
    + geom_point(
      data = df %>% filter(value_type == value_type_labels[3]),
      mapping = aes(y = value, colour = value_type), 
      shape = 1, size = 2
    )
    # simulation from fitted model
    + geom_ribbon(
      data = df %>% filter(value_type == value_type_labels[1]),
      mapping = aes(ymin = lwr, ymax = upr,
                    fill = value_type), alpha = 0.3
    )
    + geom_line(
      data = df %>% filter(value_type == value_type_labels[1]),
      mapping = aes(y = value, colour = value_type),
      linewidth = 1.25
    )
    + scale_colour_manual(values = colour_palette,
                          limits = value_type_labels)
    + scale_fill_manual(values = colour_palette,
                        limits = value_type_labels)
    + labs(title = "Incidence over time")
    + guides(
      color =
        guide_legend(override.aes = list(
          shape = c(NA, 1, 1),
          linewidth = c(1.25, NA, NA),
          fill = c(colour_palette[[1]], NA, NA))
        )
    )
    + theme(
      axis.title = element_blank(),
      legend.title = element_blank(),
      legend.justification = c(0,1),
      legend.position = c(0,1),
      legend.background = element_rect(fill = NA),
      legend.key = element_rect(fill = NA)
    )
  )
}

# fit_ensemble_summary = (model_fit
#                         %>% ensemble_stan(n_cores = 4) # generate ensemble in parallel
#                         %>% summarise_ensemble_stan()
# )

## slow!
# fcst_ensemble_summary = (model_fit
#   %>% ensemble_stan(
#    days_to_forecast = 30, # new! number of days to forecast
#    n_cores = 4
#   )
#   %>% summarise_ensemble_stan() # can specify a different quantile vector here: see documentation
# )

# plot_ensemble(fit_ensemble_summary, observed)
# plot_ensemble_val(fit_ensemble_summary, observed, validation)

#| label: def-new-beta
beta_new = 0.5

#| label: param-schedule

param_schedule = data.frame(
  Date = as.Date('2020-03-25'),
  Symbol = 'beta',
  Value = beta_new,
  Type = 'rel_orig'
)

#| label: seir-tv

seir_tv = (seir_obs_err_inc
           %>% update_piece_wise(param_schedule)
)

#| label: fig_seir-tv

seir_tv_result = simulation_history(seir_tv, obs_err = TRUE)

#| label: obs-tv

observed_tv = (
  seir_tv_result
  %>% transmute(
    date = Date,
    incidence
  )
  %>% pivot_longer(-date, names_to = "var")
  %>% filter(var == "incidence")
  %>% slice(-1)
)

#| label: fit-tv

model_fit_tv = calibrate_stan(
  model = seir_tv, # original model object
  model_to_calibrate = seir_tv_to_calibrate, # model object with observed data and priors
  chains = 2 # number of MCMC chains
)

#| label: fit-tv-diagnostics

fit = tidy_fit_stan(model_fit_tv)$fit # a simple utility to attach parameter names to stan output
rstan::summary(fit)$summary
rstan::traceplot(fit, ncol = 1)

#| label: fig_fit-tv

fit_ensemble_tv = (model_fit_tv
                        %>% ensemble_stan(n_cores = 4) # generate ensemble in parallel
                        %>% summarise_ensemble_stan()
)

plot_ensemble(fit_ensemble_tv, observed)

#| label: fig_fcst-ens-summary

fcst_ensemble_tv = (model_fit_tv
                    %>% ensemble_stan(
                      days_to_forecast = 30, # new! number of days to forecast
                      n_cores = 4
                    )
                    %>% summarise_ensemble_stan() # can specify a different quantile vector here: see documentation
)

plot_ensemble(fcst_ensemble_tv, observed)

#####

beta_new_m <- c(0.5, 0.7)
dates_masking <- c(as.Date('2020-03-25'),as.Date('2020-04-15'))

param_schedule = data.frame(
  Date = dates_masking,
  Symbol = 'beta',
  Value = beta_new_m,
  Type = 'rel_prev'
)

seir_tv = (seir_obs_err_inc
           %>% update_piece_wise(param_schedule)
)

seir_tv_result = simulation_history(seir_tv, obs_err = TRUE)

observed_tv = (
  seir_tv_result
  %>% transmute(
    date = Date, 
    incidence
  )
  %>% pivot_longer(-date, names_to = "var")
  %>% filter(var == "incidence")
  %>% slice(-1)
)

model_fit_tv = calibrate_stan(
  model = seir_tv, # original model object
  model_to_calibrate = seir_tv_to_calibrate, # model object with observed data and priors
  chains = 2 # number of MCMC chains
)

fit = tidy_fit_stan(model_fit_tv)$fit # a simple utility to attach parameter names to stan output
rstan::summary(fit)$summary
rstan::traceplot(fit, ncol = 1)

fit_ensemble_tv_m = (model_fit_tv
                   %>% ensemble_stan(n_cores = 4)
                   %>% summarise_ensemble_stan()
)

plot_ensemble(fit_ensemble_tv, observed)
plot_ensemble_val(fit_ensemble_tv, observed, validation)

fcst_ensemble_tv_m = (model_fit_tv
                    %>% ensemble_stan(
                      days_to_forecast = 30, # new! number of days to forecast
                      n_cores = 4
                    )
                    %>% summarise_ensemble_stan() # can specify a different quantile vector here: see documentation
)

plot_ensemble(fit_ensemble_tv_m, observed) +
  theme(text=element_text(size=24))
plot_ensemble_val(fcst_ensemble_tv_m, observed, validation) +
  theme(text=element_text(size=24))

plot_ensemble_val_m <- function(ens, mask, obs, val){
  value_type_labels <- c("Model fit", "Model fit with masking", "Observed data","Validation data")
  df = (ens
        %>% filter(var == "incidence")
        %>% mutate(value_type = value_type_labels[1])
        %>% bind_rows(
          mask
          %>% filter(var == "incidence")
          %>% mutate(value_type = value_type_labels[2])
        )
        %>% bind_rows(
          obs %>% mutate(value_type = value_type_labels[3])
        )
        %>% bind_rows(
          val %>% mutate(value_type = value_type_labels[4])
        )
  )
  
  colour_palette = c("dodgerblue","gold","black","red")
  names(colour_palette) = value_type_labels
  
  (ggplot(df, aes(x = date))
    # observed points
    + geom_point(
      data = df %>% filter(value_type == value_type_labels[3]),
      mapping = aes(y = value, colour = value_type), 
      shape = 1, size = 2
    )
    # observed points
    + geom_point(
      data = df %>% filter(value_type == value_type_labels[4]),
      mapping = aes(y = value, colour = value_type), 
      shape = 1, size = 2
    )
    # simulation from fitted model
    + geom_ribbon(
      data = df %>% filter(value_type == value_type_labels[1]),
      mapping = aes(ymin = lwr, ymax = upr,
                    fill = value_type), alpha = 0.3
    )
    + geom_line(
      data = df %>% filter(value_type == value_type_labels[1]),
      mapping = aes(y = value, colour = value_type),
      linewidth = 1.25
    )
    # simulation from fitted model with masks!!
    + geom_ribbon(
      data = df %>% filter(value_type == value_type_labels[2]),
      mapping = aes(ymin = lwr, ymax = upr,
                    fill = value_type), alpha = 0.3
    )
    + geom_line(
      data = df %>% filter(value_type == value_type_labels[2]),
      mapping = aes(y = value, colour = value_type),
      linewidth = 1.25
    )
    + scale_colour_manual(values = colour_palette,
                          limits = value_type_labels)
    + scale_fill_manual(values = colour_palette,
                        limits = value_type_labels)
    + labs(title = "Incidence over time")
    + guides(
      color =
        guide_legend(override.aes = list(
          shape = c(NA, NA, 1, 1),
          linewidth = c(1.25, 1.25, NA, NA),
          fill = c(colour_palette[[1]], colour_palette[[2]], NA, NA))
        )
    )
    + theme(
      axis.title = element_blank(),
      legend.title = element_blank(),
      legend.justification = c(0,1),
      legend.position = c(0,1),
      legend.background = element_rect(fill = NA),
      legend.key = element_rect(fill = NA)
    )
  )
}

plot_ensemble_val_m(fcst_ensemble_tv, fcst_ensemble_tv_m, observed, validation) +
  theme(text=element_text(size=32))
