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


