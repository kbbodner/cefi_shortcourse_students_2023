## Setting up the model

# We run the following to compile the model

model <- stan_model(here(
  "./CaseStudy_Covid19/covid-bc/team-3/stan-models/mask_seir_model.stan"
))

# Note what is provided in the "data" block of the Stan code. That is what
# is expected as input at inference time. This can be provided as a `list`
# object.

stan_data <- list(
  T = length(fitting_time_points), # number of data points
  y = fitting_cases, # observed infection cases
  ts = fitting_time_points, # Time points
  forecast_T = length(forecast_time_points), # number of forecast points
  forecast_ts = forecast_time_points, # Forecast time points
  R0_prior = c(log(2.5), 0.2), # log Mean and std for R0
  i0_prior = c(log(8), 1.0), # log Mean and std for i0
  gamma = 1 / 7, # recovery rate
  sigma = 1 / 5, # incubation rate
  pop_size = 5e6, # population of BC
  t0 = -1
)

# Run the inference
fit <- sampling(model,
  data = stan_data,
  iter = 500,
  seed = 42, # fix seed to recreate results
  chains = 2,
  control = list(adapt_delta = 0.9)
)

# Examine the summary of the posterior
summary(fit, c("R0", "i0", "sample_frac"))

## Examining the model output

# Compare the output of the different states in the model

state_dictionary <- tribble(
  ~i, ~state,
  1, "S",
  2, "E",
  3, "I",
  4, "R"
)

time_dictionary <- bc_data %>%
  select(Reported_Date) %>%
  mutate(t = 1:n())

fit %>%
  spread_draws(y_hat[t, i]) %>%
  left_join(state_dictionary, by = "i") %>%
  left_join(time_dictionary, by = "t") %>%
  filter(state == "I") %>%
  ggplot(aes(x = Reported_Date)) +
  stat_lineribbon(aes(y = y_hat), .width = c(.99, .95, .8, .5)) +
  scale_fill_brewer()
ggsave(here("./CaseStudy_Covid19/covid-bc/team-3/figures/mask_model.png"),
       width=8, height=6, units="in")

# Compare posterior predictive distribution of cases to actual cases,

time_dictionary <- bc_data %>%
  select(Reported_Date) %>%
  mutate(t = 1:n())

fit %>%
  spread_draws(cases[t]) %>%
  left_join(time_dictionary, by = "t") %>%
  ggplot(aes(x = Reported_Date)) +
  stat_lineribbon(aes(y = cases), .width = c(.99, .95, .8, .5), 
                  color = "#08519C") +
  scale_fill_brewer() +
  geom_point(data = bc_data, aes(x = Reported_Date, y = n))
ggsave(here("./CaseStudy_Covid19/covid-bc/team-3/figures/mask_posterior.png"),
       width=8, height=6, units="in")

# Compare the forecasted predictive distribution of cases to actual cases,

time_dictionary_fitting <- bc_data %>%
  select(Reported_Date) %>%
  mutate(
    t = 1:n(),
    .variable = "cases"
  )
time_dictionary_forecasting <- tibble(Reported_Date = forecast_dates) %>%
  mutate(
    t = 1:n(),
    .variable = "forecasted_cases"
  )

time_dictionary <- time_dictionary_fitting %>%
  bind_rows(time_dictionary_forecasting)

fit %>%
  tidybayes::gather_draws(cases[t], forecasted_cases[t]) %>%
  left_join(time_dictionary, by = c("t", ".variable")) %>%
  ggplot(aes(x = Reported_Date)) +
  stat_lineribbon(aes(y = .value, group = .variable),
    color = "#08519C",
    .width = c(.8, .5)
  ) +
  scale_fill_brewer() +
  geom_point(data = bc_data, aes(x = Reported_Date, y = n))
ggsave(here("./CaseStudy_Covid19/covid-bc/team-3/figures/mask_forecasting.png"),
       width=8, height=6, units="in")
