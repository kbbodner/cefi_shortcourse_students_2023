# purpose: load libraries and packages


## libraries


# mandatory setup for working with macpan and stan
# - - - - - - - - - - - - - - - - - - - - - - - - - - - 
library(McMasterPandemic)
source(here::here("R", "tmbstan_tools.R")) # some custom tools for connecting McMasterPandemic and Stan, they're documented fully in that file
#source("R/tmbstan_tools.R")
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


library(here)
library(readr)


#------------------------------------------------------------

# functions

# plotting simulation results
plot_sim = function(model, sim_result, title){
  (sim_result
   # drop any columns where a flow rate is returned
   %>% select(-matches("to"))
   # switch to long form for ease of use with ggplot
   %>% pivot_longer(-Date, names_to = "State", values_to = "Population")
   # sort state into a factor variable to enforce legend ordering based on
   # flow through the model
   %>% mutate(State = factor(State, 
                             levels = order_vars(model)))
   %>% ggplot(aes(x = Date, y = Population, colour = State))
   + geom_line(linewidth = 1.25)
   + labs(title = title)
   + theme(
     axis.title.x = element_blank(),
     legend.justification = c(1,1),
     legend.position = c(1,1),
     legend.background = element_rect(fill = NA),
     legend.key = element_rect(fill = NA)
   )
  )
}



#plotting ensemble fit
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


# This function takes:
#   1. predicted vector
#   2. observed vector
#   3. dates vector, same length as predicted and observed vector
#   4. iter_name char string, to name the output. If left alone, will provide time/date

calculate_model_errors <- function(merrIn, first_date, iter_name = "")
  # pass in predicted values, observed values, date (aligned with measures) 
{
  bd <- merrIn
  # names(bd) <- c("predicted", "observed", "dates")
  # # filter to only values that are at the start of observation
  # calculate error, defined as predicted minus observed
  bd<- bd %>% mutate(error = (bd$predicted-bd$observed)) %>% dplyr::filter(dates >= first_date)
  # calculates mean of absolute difference of sequential observations (scaling factor)
  Q = mean(abs(diff(bd$error)))
  
  # mean absolute error is the mean of the absolute error across the data
  MAE = mean(abs(bd$error))
  
  # root mean square error is the mean of the squared error across the data
  RMSE = sqrt(mean((bd$error)^2))
  
  # MASE is the mean absolute error, scaled by the "null" error Q
  MASE = MAE/Q
  
  iter_name <- if(iter_name == "") {Sys.time()}
  
  return(tribble(
    ~MAE, ~RMSE, ~MASE, ~Q, ~model_iteration,
    MAE,  RMSE,  MASE,  Q,  iter_name
  ))
}
