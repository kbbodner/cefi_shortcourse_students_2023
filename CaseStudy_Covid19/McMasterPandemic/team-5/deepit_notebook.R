# deepit notepad

# try model run
library("here")
source(here::here("team-5/utilities.R"))
source(here::here("team-5/models.R"))
source(here::here("team-5/pull_data.R"))

## basic SEIR model
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

# dates
start_date = "2020-03-01"
end_date = "2020-05-01"



seir_result = simulation_history(seir)

head(seir_result)


plot_sim(seir, seir_result,
         title = "Deterministic SEIR simulation")


plot_sim


