# make time-series plots,
# with changed gamma and alpha, 
# and mask mandate with 75% effectiveness (reduction in beta),
# and 50% adherence (reduction in beta) 


# try model run
library("here")
source(here::here("team-5/utilities.R"))

# initial state, in units of individuals
state = c(
  S = 14865000, #S = susceptible
  E = 0,  #E = exposed,
  I = 10,  #I = infected, first date of infection/known exposure is 2020-02-08
  R = 100 #R = recovered
)

# parameters -- these are new gamma and alpha 
params = c(
  beta = 0.25, #1/4
  N = sum(state),
  alpha = 0.2, #1/5
  gamma = 0.25 #1/4
)


source(here::here("team-5/models.R"))
source(here::here("team-5/pull_data.R"))

# basic SEIR model


# # dates
# start_date = "2020-03-01"
# end_date = "2020-05-01"
# 


seir_result = simulation_history(seir)



plot_sim(seir, seir_result,
         title = "Deterministic SEIR simulation")


set.seed(15)

seir_obs_err_result = (seir_obs_err
                       # turn on observation error in simluation
                       %>% simulation_history(obs_error = TRUE)
)

plot_sim(seir_obs_err, seir_obs_err_result,
         title = "Stochastic SEIR simulation")

# 
# library(lubridate)
# random_timevar = data.frame(
#   Date = ymd(20200331),
#   Symbol = 'beta',
#   Value = 0.75,
#   Type = 'rel_orig'
# )
# 
# random_timevar



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

#plot stochastic
plot_sim(seir_obs_err_inc, seir_obs_err_inc_result,
         title = "Stochastic SEIR simulation with incidence")




# create simulated data

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



# calibrate model to mock data

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


# library(McMasterPandemic)
model_fit = calibrate_stan(
  model = seir_obs_err_inc, # original model object
  model_to_calibrate = seir_obs_err_inc_to_calibrate, # model object with observed data and priors
  chains = 4 # number of MCMC chains
)

# now look at model fit:

fit = tidy_fit_stan(model_fit)$fit # a simple utility to attach parameter names to stan output
rstan::summary(fit)$summary
rstan::traceplot(fit, ncol = 1)

exp(rstan::summary(fit)$summary["log_beta", "mean"])
params[["beta"]]

# model fit
fit_ensemble_summary = (model_fit
                        %>% ensemble_stan(n_cores = 4) # generate ensemble in parallel
                        %>% summarise_ensemble_stan()
)

head(fit_ensemble_summary)

plot_ensemble(fit_ensemble_summary, cases2)
ggsave(here::here("team-5/img/fig2_fit_deepit.png"), height = 9, width = 16, units = "cm")


# forecast:


fcst_ensemble_summary = (model_fit
                         %>% ensemble_stan(
                           days_to_forecast = 30, # new! number of days to forecast
                           n_cores = 4
                         )
                         %>% summarise_ensemble_stan() # can specify a different quantile vector here: see documentation
)

plot_ensemble(fcst_ensemble_summary, cases2)
ggsave(here::here("team-5/img/fig2_pred_deepit.png"), height = 9, width = 16, units = "cm")
View(fcst_ensemble_summary)



#### now let's try editing beta so that mask wearing is incorporated.'
### How? reduce beta by 25%


state = c(
  S = 14865000, #S = susceptible
  E = 0,  #E = exposed,
  I = 10,  #I = infected, first date of infection/known exposure is 2020-02-08
  R = 100 #R = recovered
)

# parameters -- these are new gamma and alpha 
params = c(
  beta = 0.25, #1/4
  N = sum(state),
  alpha = 0.2, #1/5
  gamma = 0.25 #1/4
)


source(here::here("team-5/models.R"))
source(here::here("team-5/pull_data.R"))

# basic SEIR model


# # dates
# start_date = "2020-03-01"
# end_date = "2020-05-01"
# 


seir_result = simulation_history(seir)



plot_sim(seir, seir_result,
         title = "Deterministic SEIR simulation")


set.seed(15)

seir_obs_err_result = (seir_obs_err
                       # turn on observation error in simluation
                       %>% simulation_history(obs_error = TRUE)
)

plot_sim(seir_obs_err, seir_obs_err_result,
         title = "Stochastic SEIR simulation")


library(lubridate)
random_timevar = data.frame(
  Date = ymd(20200401),
  Symbol = 'beta',
  Value = 0.75,
  Type = 'rel_orig'
)

random_timevar



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

#plot stochastic
plot_sim(seir_obs_err_inc, seir_obs_err_inc_result,
         title = "Stochastic SEIR simulation with incidence")




# create simulated data

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



# calibrate model to mock data

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


# library(McMasterPandemic)
model_fit = calibrate_stan(
  model = seir_obs_err_inc, # original model object
  model_to_calibrate = seir_obs_err_inc_to_calibrate, # model object with observed data and priors
  chains = 4 # number of MCMC chains
)

# now look at model fit:

fit = tidy_fit_stan(model_fit)$fit # a simple utility to attach parameter names to stan output
rstan::summary(fit)$summary
rstan::traceplot(fit, ncol = 1)

exp(rstan::summary(fit)$summary["log_beta", "mean"])
params[["beta"]]

# model fit
fit_ensemble_summary = (model_fit
                        %>% ensemble_stan(n_cores = 4) # generate ensemble in parallel
                        %>% summarise_ensemble_stan()
)

head(fit_ensemble_summary)

plot_ensemble(fit_ensemble_summary, cases2) + labs(title = "Incidence over time -- mask wearing starting Apr 1")
ggsave(here::here("team-5/img/fig3_fit.png"), height = 9, width = 16, units = "cm")


# forecast:


fcst_ensemble_summary = (model_fit
                         %>% ensemble_stan(
                           days_to_forecast = 30, # new! number of days to forecast
                           n_cores = 4
                         )
                         %>% summarise_ensemble_stan() # can specify a different quantile vector here: see documentation
)

plot_ensemble(fcst_ensemble_summary, cases2) + labs(title = "Incidence over time -- mask wearing starting Apr 1")
ggsave(here::here("team-5/img/fig3_pred.png"), height = 9, width = 16, units = "cm")
View(fcst_ensemble_summary)

