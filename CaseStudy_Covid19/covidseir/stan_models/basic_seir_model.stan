// This Stan program defines a SEIR model with observed cases y at time-points
// t. log-normal priors are specified for R0 and i0 and observations are 
// Poisson distributed
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started

// The function block defines all custom functions used in the stan file
// Functions can also be added by using the #include tag at the top of the file
functions {
  real[] seir_rhs(real t, // time (actual time; not an increment starting at 1)
             real[] y,  // state
             real[] theta,  // parameters
             real[] x_r,    // data (real)
             int[]  x_i) {  // data (integer)
    real dydt[4];
    
    // define parameters.
    // any non-fixed parameters are in the theta array. Any fixed
    // parameters are in the x_r array
    real R0 = theta[1];
    real sigma = x_r[1];
    real gamma = x_r[2];
    real pop_size = x_r[3];
    
    real lambda = (R0 * gamma * y[3]) / pop_size;
    
    // S
    dydt[1] = - lambda * y[1];
    // E
    dydt[2] =  lambda * y[1] - sigma * y[2];
    // I
    dydt[3] =  sigma * y[2] - gamma * y[3];
    // R
    dydt[4] =  gamma * y[3];
  
    return dydt;
  }
}
// the data block defines all variables that are passed into stan from R
// angle-brackets <> denote any restrictions applied to the data for example
// <lower=0> means an error would be generated if negative values are included
data {
  int<lower=1> T;
  int<lower=0> forecast_T; //length of number of forecast days
  real t0;
  real ts[T]; // days for which data are recorded (index starting at t0) 
  real forecast_ts[forecast_T]; // days to forecast
  int<lower=0> y[T];      // Observed infection cases
  real<lower=0> R0_prior[2]; // log Mean and std for R0
  real<lower=0> i0_prior[2];  // log Mean and std for i0
  real<lower=0> gamma;  // recovery rate
  real<lower=0> sigma;  // incubation rate
  real<lower=0> pop_size;  // Total population size
}
// The transformed data block defines any variables that take the data and 
// do something with it, but will be fixed and not affected by any parameters
// This ensures computation in this block only runs once and not every time
// any parameter is updated in an MCMC step.
transformed data {
  real x_r[3];
  int x_i[0];
  
  
  x_r[1] = gamma;
  x_r[2] = sigma;
  x_r[3] = pop_size;
}
// Parameters are anything that should be treated as a random variable
// Priors for these variables are defined in the model block. If they're not 
// then an improper prior is assumed
parameters {
  real R0;
  real i0;
  real<lower=0, upper=1> sample_frac;
}
// Transformed parameters stores any variables that depends on anything
// defined in the parameters block. For example since the number of initial
// infected is treated as a random variable, the set of initial conditions (y0)
// is also necessarily treated as a random variable
transformed parameters {
  real theta[1];
  real y0[4];
  real y_hat[T,4];
  
  // set the initial conditions of the ODE
  y0[1] = pop_size - i0;
  y0[2] = 0;
  y0[3] = i0;
  y0[4] = 0;
  
  // Pass any SEIR parameters that are not fixed into the theta vector
  theta[1] = R0;
  
  // integrate the seir_rhs function in time and return values at ts time-points
  y_hat = integrate_ode_rk45(seir_rhs, y0, t0, ts, theta, x_r, x_i);
}
// The model block includes definitions for the priors and likelihood
model {
  
  
  // Priors
  R0 ~ lognormal(R0_prior[1], R0_prior[2]);
  i0 ~ lognormal(i0_prior[1], i0_prior[2]);
  sample_frac ~ normal(0.2,0.01);
  
  // likelihood
  y ~ poisson(sample_frac * col(to_matrix(y_hat), 3));
}

// Generated quantities are produced for each sample of the posterior and 
// are typically used for generating the posterior predictive distribution.
// We generate the posterior predictive distribution of the cases and the 
// and the forecasted cases.
generated quantities{
  int cases[T];
  int forecasted_cases[forecast_T];
  real y0_forecast[4];
  real y_forecast[forecast_T,4];
  
  // generate the output of each state and the cases for 
  // each sample of the posterior
  cases = poisson_rng(sample_frac * col(to_matrix(y_hat), 3));
  
  // generate output for the forecasted days
  // new initial conditions are for the final day of inference
  y0_forecast = to_array_1d(row(to_matrix(y_hat), T));
  // integrate forward from final day of inference
  y_forecast = integrate_ode_rk45(seir_rhs, y0_forecast, ts[T], forecast_ts, theta, x_r, x_i);
  forecasted_cases = poisson_rng(sample_frac * col(to_matrix(y_forecast), 3));
}
