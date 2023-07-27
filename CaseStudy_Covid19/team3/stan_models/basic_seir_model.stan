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
    real pop_size = x_r[3]; // This is now a vector, maybe we need a x_r for each region
    
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
  int<lower=0> y1[T];      // Observed infection cases region 1
  int<lower=0> y2[T];      // Observed infection cases region 2
  int<lower=0> y3[T];      // Observed infection cases region 3
  int<lower=0> y4[T];      // Observed infection cases region 4
  int<lower=0> y5[T];      // Observed infection cases region 5
  real<lower=0> mu_R0_vec_prior[2]; // log Mean and std for mu_R0
  real<lower=0> sd_R0_vec_prior[2]; // log Mean and std for sd_R0
  real<lower=0> mu_i0_vec_prior[2]; // log Mean and std for mu_i0
  real<lower=0> sd_i0_vec_prior[2]; // log Mean and std for sd_i0
  real<lower=0> gamma;  // recovery rate
  real<lower=0> sigma;  // incubation rate
  real<lower=0> pop_size[5];  // Total population size
  int<lower=0> n_sites; // Number of regions in BC
}
// The transformed data block defines any variables that take the data and 
// do something with it, but will be fixed and not affected by any parameters
// This ensures computation in this block only runs once and not every time
// any parameter is updated in an MCMC step.
transformed data {
  real x_r1[3];
  real x_r2[3];
  real x_r3[3];
  real x_r4[3];
  real x_r5[3];
  int x_i[0];
  
  
  x_r1[1] = gamma;   // x_r vec for region 1
  x_r1[2] = sigma;
  x_r1[3] = pop_size[1]; 
  
  x_r2[1] = gamma; // x_r vec for region 2
  x_r2[2] = sigma;
  x_r2[3] = pop_size[2]; 
  
  x_r3[1] = gamma; // x_r vec for region 3
  x_r3[2] = sigma;
  x_r3[3] = pop_size[3]; 
  
  x_r4[1] = gamma; // x_r vec for region 4
  x_r4[2] = sigma;
  x_r4[3] = pop_size[4]; 
  
  x_r5[1] = gamma; // x_r vec for region 5
  x_r5[2] = sigma;
  x_r5[3] = pop_size[5]; 
  
  // Pop size is now a vector and we need to extract the appropriate regional value ----
  // Should I put this in the model loop?
}
// Parameters are anything that should be treated as a random variable
// Priors for these variables are defined in the model block. If they're not 
// then an improper prior is assumed
parameters {
  real<lower=0> R0[n_sites]; // Make R0 a vector of 5, accounting for the number of regions ----
  real<lower=0> i0[n_sites]; // Make i0 a vector of 5, accounting for the number of regions ----
  real<lower=0, upper=1> sample_frac;
  real<lower=0> phi;
  real<lower=0> mu_R0;
  real<lower=0> sd_R0;
  real<lower=0> mu_i0;
  real<lower=0> sd_i0;
}
// Transformed parameters stores any variables that depends on anything
// defined in the parameters block. For example since the number of initial
// infected is treated as a random variable, the set of initial conditions (y0)
// is also necessarily treated as a random variable
transformed parameters {
  real theta[1]; 
  real y0[4];
  real y_hat1[T,4];
  real y_hat2[T,4];
  real y_hat3[T,4];
  real y_hat4[T,4];
  real y_hat5[T,4];
  
  
  // integrate the seir_rhs function in time and return values at ts time-points
for(i in 1:n_sites){
    
  // Pass any SEIR parameters that are not fixed into the theta vector
  theta[1] = R0[i]; 
  
  // set the initial conditions of the ODE
  y0[1] = pop_size[i] - i0[i];
  y0[2] = 0;
  y0[3] = i0[i];
  y0[4] = 0;
  
  if(i == 1){
    y_hat1 = integrate_ode_rk45(seir_rhs, y0, t0, ts, theta, x_r1, x_i); // run SEIR model for each region 
  }
  
  if(i == 2){
    y_hat2 = integrate_ode_rk45(seir_rhs, y0, t0, ts, theta, x_r2, x_i); 
  }
  
  if(i == 3){
    y_hat3 = integrate_ode_rk45(seir_rhs, y0, t0, ts, theta, x_r3, x_i); 
  }
  
  if(i == 4){
    y_hat4 = integrate_ode_rk45(seir_rhs, y0, t0, ts, theta, x_r4, x_i); 
  }
  
  if(i == 5){
    y_hat5 = integrate_ode_rk45(seir_rhs, y0, t0, ts, theta, x_r5, x_i); 
  }
  
  
  }
}
// The model block includes definitions for the priors and likelihood
model {
  
  // We need a hyperprior for our random effects
  
  // Hyperprior for R0
  mu_R0 ~ std_normal();// normal(mu_R0_vec_prior[1], mu_R0_vec_prior[2]); // Global mean of R0 to go into a lognormal
  sd_R0 ~ std_normal();// gamma(sd_R0_vec_prior[1], sd_R0_vec_prior[2]); // Global sd of R0 to go into a lognormal 
  
  //Hyperprior for i0
  mu_i0 ~ std_normal();// normal(mu_i0_vec_prior[1], mu_i0_vec_prior[2]); // Global mean of i0 to go into a lognormal 
  sd_i0 ~ std_normal();// gamma(sd_i0_vec_prior[1], sd_i0_vec_prior[2]); // Global sd of i0 to go into a lognormal 
  
  // Priors 
  
  // We now need to have a prior for each R0, that is governed by the global mean and sd defined in our hyperpriors 
  
  for(i in 1:n_sites){
    
    R0[i] ~ normal(mu_R0, sd_R0); 
    
  }
  
  // We now need to have a prior for each i0, that is governed by the global mean and sd defined in our hyperpriors 
  
  for(i in 1:n_sites){
    
    i0[i] ~ normal(mu_i0, sd_i0); 
    
  }
  
  sample_frac ~ normal(0.2,0.01);
  
  1/phi ~ cauchy(0, 10); // prior choice recommendations on STAN, needs to be somewhat informative 
  
  // likelihood
  // y ~ poisson(sample_frac * col(to_matrix(y_hat), 3)); // maybe make a negative binomial, negbin2 for mean and overdispersion
  
  for(i in 1:n_sites){
 
  if(i == 1){   
    y1 ~ neg_binomial_2(sample_frac * col(to_matrix(y_hat1), 3), phi);
  }
 
  if(i == 2){
    y2 ~ neg_binomial_2(sample_frac * col(to_matrix(y_hat2), 3), phi);
  }
  
  if(i == 3){
    y3 ~ neg_binomial_2(sample_frac * col(to_matrix(y_hat3), 3), phi);
  }
  
  if(i == 4){
    y4 ~ neg_binomial_2(sample_frac * col(to_matrix(y_hat4), 3), phi);
  }
  
  if(i == 5){
    y5 ~ neg_binomial_2(sample_frac * col(to_matrix(y_hat5), 3), phi);
  }
  
  }
}

// Generated quantities are produced for each sample of the posterior and 
// are typically used for generating the posterior predictive distribution.
// We generate the posterior predictive distribution of the cases and the 
// and the forecasted cases.
generated quantities{
  int cases1[T];
  int cases2[T];
  int cases3[T];
  int cases4[T];
  int cases5[T];
  
  int forecasted_cases1[forecast_T];
  int forecasted_cases2[forecast_T];
  int forecasted_cases3[forecast_T];
  int forecasted_cases4[forecast_T];
  int forecasted_cases5[forecast_T];
  
  real y0_forecast1[4];
  real y0_forecast2[4];
  real y0_forecast3[4];
  real y0_forecast4[4];
  real y0_forecast5[4];
  
  real y_forecast1[forecast_T,4];
  real y_forecast2[forecast_T,4];
  real y_forecast3[forecast_T,4];
  real y_forecast4[forecast_T,4];
  real y_forecast5[forecast_T,4];
  
  real theta_forecast1[1];
  real theta_forecast2[1];
  real theta_forecast3[1];
  real theta_forecast4[1];
  real theta_forecast5[1];
  
  
    // Pass any SEIR parameters that are not fixed into the theta vector
  theta_forecast1[1] = R0[1]*0.5; 
  theta_forecast2[1] = R0[2]*0.5; 
  theta_forecast3[1] = R0[3]*0.5;
  theta_forecast4[1] = R0[4]*0.5;
  theta_forecast5[1] = R0[5]*0.5; 
  
  
  // generate the output of each state and the cases for 
  // each sample of the posterior

  cases1 = neg_binomial_2_rng(sample_frac * col(to_matrix(y_hat1), 3), phi);
  
  cases2 = neg_binomial_2_rng(sample_frac * col(to_matrix(y_hat2), 3), phi);
  
  cases3 = neg_binomial_2_rng(sample_frac * col(to_matrix(y_hat3), 3), phi);
  
  cases4 = neg_binomial_2_rng(sample_frac * col(to_matrix(y_hat4), 3), phi);
  
  cases5 = neg_binomial_2_rng(sample_frac * col(to_matrix(y_hat5), 3), phi);
  
  // generate output for the forecasted days
  // new initial conditions are for the final day of inference
  
  y0_forecast1 = to_array_1d(row(to_matrix(y_hat1), T));
  
  y0_forecast2 = to_array_1d(row(to_matrix(y_hat2), T));
  
  y0_forecast3 = to_array_1d(row(to_matrix(y_hat3), T));
  
  y0_forecast4 = to_array_1d(row(to_matrix(y_hat4), T));
  
  y0_forecast5 = to_array_1d(row(to_matrix(y_hat5), T));
  
  // integrate forward from final day of inference
  
  y_forecast1 = integrate_ode_rk45(seir_rhs, y0_forecast1, ts[T], forecast_ts, theta_forecast1, x_r1, x_i);
  
  y_forecast2 = integrate_ode_rk45(seir_rhs, y0_forecast2, ts[T], forecast_ts, theta_forecast2, x_r2, x_i);
  
  y_forecast3 = integrate_ode_rk45(seir_rhs, y0_forecast3, ts[T], forecast_ts, theta_forecast3, x_r3, x_i);
  
  y_forecast4 = integrate_ode_rk45(seir_rhs, y0_forecast4, ts[T], forecast_ts, theta_forecast4, x_r4, x_i);
  
  y_forecast5 = integrate_ode_rk45(seir_rhs, y0_forecast5, ts[T], forecast_ts, theta_forecast5, x_r5, x_i);
  
  
  //forecasted_cases = poisson_rng(sample_frac * col(to_matrix(y_forecast), 3));
  
  forecasted_cases1 = neg_binomial_2_rng(sample_frac * col(to_matrix(y_forecast1), 3), phi);
  
  forecasted_cases2 = neg_binomial_2_rng(sample_frac * col(to_matrix(y_forecast2), 3), phi);
  
  forecasted_cases3 = neg_binomial_2_rng(sample_frac * col(to_matrix(y_forecast3), 3), phi);
  
  forecasted_cases4 = neg_binomial_2_rng(sample_frac * col(to_matrix(y_forecast4), 3), phi);
  
  forecasted_cases5 = neg_binomial_2_rng(sample_frac * col(to_matrix(y_forecast5), 3), phi);
  
}

