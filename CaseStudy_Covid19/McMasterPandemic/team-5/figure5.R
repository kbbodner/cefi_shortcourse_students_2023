# DEEPIT n*n matrix of scenarios

# first we create a function to take in the early data, fit a model to the early data, 
# and then output the number of cases averted compared to the base case (fig 2). 
# Maybe we can use maximum incidence or reduction in incidence total compared to figure 2
# 
# inputs:
#   1. filename
#   2. plot_title
#   3. beta_change
#   
#   
#   
#   
#   
cases2 <- cases %>%
    mutate(var = "incidence") %>%
    rename(value = new_cases) %>%
    relocate(value, .after = var) %>% filter(date >= "2020-02-09")



cases_val <- read_csv(here("data", "validation.csv"))     %>% mutate(var = "incidence") %>%
  rename(value = new_cases) %>%
  relocate(value, .after = var) %>% filter(date >= "2020-02-09")

source(here::here("team-5/models.R"))
source(here::here("team-5/pull_data.R"))
source(here::here("team-5/utilities.R"))
state = c(
  S = 14865000, #S = susceptible
  E = 0,  #E = exposed,
  I = 10,  #I = infected, first date of infection/known exposure is 2020-02-08
  R = 100 #R = recovered
)

# parameters -- these are new gamma and alpha 
params = c(
  beta = 0.575, #1/4
  N = sum(state),
  alpha = 0.2, #1/5
  gamma = 0.25 #1/4
)
# 

seir_result = simulation_history(seir)
set.seed(15)


seir_obs_err = (seir
                %>% update_error_dist(
                  S ~ poisson(),
                  E ~ poisson(),
                  I ~ poisson(),
                  R ~ poisson()
                )
)

seir_obs_err_result = (seir_obs_err
                       # turn on observation error in simluation
                       %>% simulation_history(obs_error = TRUE)
)


plot_sim(seir_obs_err, seir_obs_err_result, "")
# adding time-varying beta for all of march
library(lubridate)
random_timevar = data.frame(
  Date = ymd(seq(from = 20200301, to = 20200330, by = 1)),
  Symbol = rep('beta', 30),
  Value = rep(NA, 30),
  Type = rep('abs', 30)
)



# update model to include incidence
seir_obs_err_inc_ = (seir_obs_err
                    # add an expression to calculate in the simulation report
                    %>% add_sim_report_expr("incidence", ~ (S_to_E) * (S))
                    # add_error_dist instead of update_ because 
                    # update_ replaces previously attached error distributions
                    # add_ appends
                    %>% add_piece_wise(random_timevar)
                    %>% add_error_dist( 
                      incidence ~ poisson()
                    ) 
                    )

seir_obs_err_inc_result_ = (seir_obs_err_inc_
                           %>% simulation_history(obs_error = TRUE)
)

plot_sim(seir_obs_err_inc_, seir_obs_err_inc_result_ %>% select(-S), "")
# example for file_name: "team-5/img/fig3"


ggplot(data = seir_obs_err_inc_result_, aes(x = Date, y = incidence)) + geom_line()

create_scenario_output <- function(file_name = "", plot_title = "", beta_change = 0.95)
{
  library(lubridate)
  # random_timevar2 = data.frame(
  #   Date = ymd(seq(from = 20200401, to = 20200407, by = 1)),
  #   Symbol = rep('beta', 7),
  #   Value = rep(beta_change, 7),
  #   Type = rep('rel_prev', 7)
  # )
  # 
  random_timevar2 = data.frame(
    Date = ymd((20200331)),
    Symbol = rep('beta', 1),
    Value = rep(beta_change^7, 1),
    Type = rep('rel_prev', 1)
  )

  # update model to include timevar_april
  seir_obs_err_inc = (seir_obs_err_inc_
                      %>% add_piece_wise(random_timevar2)
  )
  
  seir_obs_err_inc_result2 = (seir_obs_err_inc
                             %>% simulation_history(obs_error = TRUE)
  )
  # 
  # #plot stochastic
  plot_sim(seir_obs_err_inc, seir_obs_err_inc_result2 %>% select(Date, incidence),
           title = "Stochastic SEIR simulation with incidence")
  
  
  
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
                                     %>% add_opt_tv_params(tv_type = "abs"
                                                           , log_beta ~ log_flat((-4))
                                     )
                                   
  )
  seir_obs_err_inc_to_calibrate$do_sim_constraint = TRUE
  
  
  # library(McMasterPandemic)
  model_fit = calibrate_stan(
    model = seir_obs_err_inc, # original model object
    model_to_calibrate = seir_obs_err_inc_to_calibrate, # model object with observed data and priors
    chains = 2, iter = 1000 # number of MCMC chains
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
  
  # head(fit_ensemble_summary)
  
  plot_ensemble(fit_ensemble_summary, cases2) + labs(title = plot_title)
  ggsave(here::here(paste0(file_name, "_fit.png")), height = 9, width = 16, units = "cm")
  
  
  # forecast:
  
  
  fcst_ensemble_summary = (model_fit
                           %>% ensemble_stan(
                             days_to_forecast = 30, # new! number of days to forecast
                             n_cores = 4
                           )
                           %>% summarise_ensemble_stan() # can specify a different quantile vector here: see documentation
  )
  
  plot_ensemble(fcst_ensemble_summary, rbind(cases2, cases_val)) + labs(title = plot_title)
  ggsave(here::here(paste0(file_name, "_pred.png")), height = 9, width = 16, units = "cm")
  
  fcst_outproc <- fcst_ensemble_summary%>%filter(var == "incidence", date <= ("2020-04-15"), date > ("2020-02-08"))
  cases_err <- ((rbind(cases2, cases_val))["value"])
  dates <- as.Date(fcst_outproc$date)
  merr_in <- cbind(fcst_outproc$value, cases_err, dates)
  names(merr_in) <- c("predicted", "observed", "dates")
  merr_in$observed - merr_in$predicted
  
  merr <- calculate_model_errors(merr_in,
                                 first_date = as.Date("2020-04-01"),
                                 iter_name = (plot_title)
                                 )
  merr$max_inc <- max((fcst_ensemble_summary %>% filter(var == "incidence"))["value"])
  # return(max((fcst_ensemble_summary %>% filter(var == "incidence"))["value"]))
  return(merr)
}

# 
# create_scenario_output(file_name = "team-5/img/_test_fig", plot_title = "_test beta change 0.95^7", beta_change = 0.95)




#test time-varying beta 

test_beta_080 <- create_scenario_output(file_name = "team-5/img/beta_080_tv", plot_title = "TV: Mask wearing 80% effective", beta_change = 0.2^(1/7))

test_beta_070 <- create_scenario_output(file_name = "team-5/img/beta_070_tv", plot_title = "TV: Mask wearing 70% effective", beta_change = 0.8419)

test_beta_060 <- create_scenario_output(file_name = "team-5/img/beta_060_tv", plot_title = "TV: Mask wearing 60% effective", beta_change = 0.877)

test_beta_050 <- create_scenario_output(file_name = "team-5/img/beta_050_tv", plot_title = "TV: Mask wearing 50% effective", beta_change = 0.905)

test_beta_040 <- create_scenario_output(file_name = "team-5/img/beta_040_tv", plot_title = "TV: Mask wearing 40% effective", beta_change = 0.6^(1/7))

test_beta_030 <- create_scenario_output(file_name = "team-5/img/beta_030_tv", plot_title = "TV: Mask wearing 30% effective", beta_change = 0.95)

test_beta_020 <- create_scenario_output(file_name = "team-5/img/beta_020_tv", plot_title = "TV: Mask wearing 20% effective", beta_change = 0.9686251)


test_beta_010 <- create_scenario_output(file_name = "team-5/img/beta_010_tv", plot_title = "TV: Mask wearing 10% effective", beta_change = 0.9^(1/7))


test_beta_010$model_iteration = "beta10"
test_beta_020$model_iteration = "beta20"

test_beta_030$model_iteration = "beta30"
test_beta_040$model_iteration = "beta40"

test_beta_050$model_iteration = "beta50"
test_beta_060$model_iteration = "beta60"

test_beta_070$model_iteration = "beta70"
test_beta_080$model_iteration = "beta80"


test_beta_out <- rbind(test_beta_010, test_beta_020, test_beta_030, test_beta_040, test_beta_050, test_beta_060,
      test_beta_070, test_beta_080)

write_csv(test_beta_out, file = here("team-5/img/beta_error_oos.csv"))

# create outputs for varying levels of mask effectiveness


# 
# max_beta_000 <- max((fig2_summarydata %>% filter(var == "incidence"))["value"])
# 
# 
# max_beta_010 <- create_scenario_output(file_name = "team-5/img/beta_010", plot_title = "Mask wearing 10% effective", beta_change = 0.9856)
# 
# max_beta_020 <- create_scenario_output(file_name = "team-5/img/beta_020", plot_title = "Mask wearing 20% effective", beta_change = 0.96862)
# 
# max_beta_030 <- create_scenario_output(file_name = "team-5/img/beta_030", plot_title = "Mask wearing 30% effective", beta_change = 0.95)
# 
# max_beta_040 <- create_scenario_output(file_name = "team-5/img/beta_040", plot_title = "Mask wearing 40% effective", beta_change = 0.929)
# 
# max_beta_050 <- create_scenario_output(file_name = "team-5/img/beta_050", plot_title = "Mask wearing 50% effective", beta_change = 0.905)
# 
# max_beta_060 <- create_scenario_output(file_name = "team-5/img/beta_060", plot_title = "Mask wearing 60% effective", beta_change = 0.877)
# 
# max_beta_070 <- create_scenario_output(file_name = "team-5/img/beta_070", plot_title = "Mask wearing 70% effective", beta_change = 0.8419)
# 
# max_beta_080 <- create_scenario_output(file_name = "team-5/img/beta_080", plot_title = "Mask wearing 80% effective", beta_change = 0.7945)
# 
# max_beta_090 <- create_scenario_output(file_name = "team-5/img/beta_090", plot_title = "Mask wearing 90% effective", beta_change = 0.719)
# 

# to do: calibrate