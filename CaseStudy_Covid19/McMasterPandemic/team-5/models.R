### model start and end dates ###
start_date = "2020-02-08"
end_date = "2020-03-31"


### seir model set up ###
seir = flexmodel(
  params = params,
  state = state,
  start_date = start_date,
  end_date = end_date,
  do_hazard = TRUE # an option to ensure computations are stable when any of the state variables approach zero (keep this switched on!)
)

### adding rate changes to seir model ###
seir = (
  seir
  %>% add_rate(
    "S", # from compartment S
    "E", # to compartment I
    ~ (1/N) * (beta) * (I) # expression for the flow in terms of parameters and state variables, as a formula
  )
  %>% add_rate("E", "I", ~ (alpha))
  %>% add_rate("I", "R", ~ (gamma))     
)

### stochastic ###
seir_obs_err = (seir
                %>% update_error_dist(
                  S ~ poisson(),
                  E ~ poisson(),
                  I ~ poisson(),
                  R ~ poisson()
                )
)



