### figure 1
### projection with original parameters and population size
### did not read from pull_data due to pre-specified state numbers and parameters

library("here")

start_date = "2020-03-01"
end_date = "2020-05-01"

# initial state, in units of individuals
state = c(
  S = 2000, 
  E = 0, 
  I = 1, 
  R = 0
)

# parameters
params = c(
  beta = 0.7,
  N = sum(state),
  alpha = 0.05,
  gamma = 0.06
)

source(here::here("./CaseStudy_Covid19/covid-on/team-5/R/team-5/utilities.R"))
source(here::here("./CaseStudy_Covid19/covid-on/team-5/R/team-5/models.R"))
cases = read_csv(here::here("./CaseStudy_Covid19/covid-on/data/fitting.csv")) 


# for reproducibility
set.seed(15)

# update model to include incidence
seir_obs_err_inc = (seir_obs_err
                    # add an expression to calculate in the simulation report
                    %>% add_sim_report_expr("incidence", ~ (S_to_E) * (S))
                    # add_error_dist instead of update_ because 
                    # update_ replaces previously attached error distributions
                    # add_ appends
                    %>% add_error_dist( 
                      incidence ~ poisson()
                    )
)

seir_obs_err_inc_result = (seir_obs_err_inc
                           %>% simulation_history(obs_error = TRUE)
)

plot_sim(seir_obs_err_inc, seir_obs_err_inc_result,
         title = "Stochastic SEIR simulation with incidence")

observed = (
  seir_obs_err_inc_result
  %>% select(-matches("to"))
  %>% rename(date = Date)
  %>% pivot_longer(-date, names_to = "var")
  # keep incidence observations
  %>% filter(var == "incidence")
  # lob off first observation 
  # (fitting the initial value is technically difficult and not important to figure out here)
  %>% slice(-1)
)

cases2 <- cases %>% 
  mutate(var = "incidence") %>% 
  rename(value = new_cases) %>% 
  relocate(value, .after = var) %>% filter(date >= "2020-02-09")


seir_obs_err_inc_to_calibrate = (seir_obs_err_inc
                                 # attach observed data
                                 %>% update_observed(
                                   cases2
                                 )
                                 # attach priors for parameters we're fitting
                                 # ("optimizing" over)
                                 %>% update_opt_params(
                                   # fitting log beta
                                   log_beta ~ log_normal(
                                     -1, # log mean, so mean beta is exp(-1) = 0.36
                                     0.5 # standard deviation of the log normal prior
                                   )
                                 )
)

model_fit = calibrate_stan(
  model = seir_obs_err_inc, # original model object
  model_to_calibrate = seir_obs_err_inc_to_calibrate, # model object with observed data and priors
  chains = 2 # number of MCMC chains
)

fit = tidy_fit_stan(model_fit)$fit # a simple utility to attach parameter names to stan output
rstan::summary(fit)$summary

fit_ensemble_summary = (model_fit
                        %>% ensemble_stan(n_cores = 4) # generate ensemble in parallel
                        %>% summarise_ensemble_stan()
)

fcst_ensemble_summary = (model_fit
                         %>% ensemble_stan(
                           days_to_forecast = 30, # new! number of days to forecast
                           n_cores = 4
                         )
                         %>% summarise_ensemble_stan() # can specify a different quantile vector here: see documentation
)

plot_ensemble(fcst_ensemble_summary, cases2)

View(fcst_ensemble_summary) 
