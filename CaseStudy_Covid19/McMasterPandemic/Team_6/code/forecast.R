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

## slow!
fcst_ensemble_summary = (model_fit
  %>% ensemble_stan(
   days_to_forecast = 30, # new! number of days to forecast
   n_cores = 4
  )
  %>% summarise_ensemble_stan() # can specify a different quantile vector here: see documentation
)