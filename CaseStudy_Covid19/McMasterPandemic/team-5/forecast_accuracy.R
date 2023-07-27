# Purpose: create a series of functions that read in model (define) 
#   objects and convert them to measures of accuracy

# Inputs:
  #   a. date
  #   b. var (incidence)
  #   c. value
  #   d. lwr, upr
  #   e. value_type
  #   f. group


# Process:

# Load input
# select measure to compare against
# using measure, calculate metric of comparison
# output measure of comparison
# 
# This function takes:
#   1. predicted vector
#   2. observed vector
#   3. dates vector, same length as predicted and observed vector
#   4. iter_name char string, to name the output. If left alone, will provide time/date

calculate_model_errors <- function(predicted, observed, dates, first_date, iter_name = "")
  # pass in predicted values, observed values, date (aligned with measures) 
{
  bd <- as.data.frame(cbind(predicted, observed, dates))
  # names(bd) <- c("predicted", "observed", "dates")
  # # filter to only values that are at the start of observation
  bd<- bd %>% mutate(error = (bd$predicted-bd$observed)) %>% dplyr::filter(dates >= first_date)
  # 
  Q = mean(abs(diff(bd$error)))

  
  MAE = mean(abs(bd$error))
  RMSE = sqrt(mean((bd$error)^2))
  MASE = MAE/Q
  
  iter_name <- if(iter_name == "") {Sys.time()}

  return(tribble(
    ~MAE, ~RMSE, ~MASE, ~Q, ~model_iteration,
     MAE,  RMSE,  MASE,  Q,  iter_name
  ))
}

# 
# TESTING THE FUNCTION
#create an index to refer to for creating mock data
index_observed <- seq(from = 0, to = 99, by = 1)
#create list of dates starting on January 1, 2020
time_obs <- as.Date("2020-01-01") + index_observed

#create vector of observation mock data
case_observed = abs(rnorm(index_observed, mean = index_observed, sd = 3))

#create vector of predicted mock data
case_predicted = abs(rnorm(case_observed, mean = case_observed, sd = 20))

# test plot (do not run)
# plot(case_observed, case_predicted)


# out dataframe
out_test_cme <- calculate_model_errors(case_predicted, case_observed, time_obs, min(time_obs) + 50)
