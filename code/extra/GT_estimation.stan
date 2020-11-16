// Code to jointly estimate the serial interval and R
// based on the methods of White and Pagano
// adapted from Truelove et al (ref) for
// use with cholera outbreak data from South Sudan

data {
  // Data inputs
  int<lower=1> T_max;                      // maximum length of time
  int N_mat[T_max];                       // all cases at each time step minus t=0
  int<lower=1> N0;                       // Initial number of cases
  
  
  // Assumed hyper-parameters related to the generation time
  real<lower=0> shape_mean;                 // Shape parameter for Generation Time gamma distribution [MEAN]
  real<lower=0> shape_sd;                   // Shape parameter for Generation Time gamma distribution [SD]
  real<lower=0> scale_mean;                 // Scale parameter for Generation Time gamma distribution [MEAN]
  real<lower=0> scale_sd;                   // Scale parameter for Generation Time gamma distribution [SD]
  
}


parameters {
  // Generation time parameters
  real<lower=0> shape;                      // Shape parameter for Generation Time gamma distribution
  real<lower=0> scale;                      // Scale of Generation Time gamma distribution
  real<lower=0> logR;                       // log R0
  
}

model {
  vector[T_max] p;                          // Generation time probability distribution
  real p_sum;
  real mu_tmp;
  vector[T_max] mu;                             


  // Priors (weakly informative)
  shape ~ normal(shape_mean, shape_sd);
  scale ~ normal(scale_mean, scale_sd);
  logR ~ normal(0, 100);                
  p = rep_vector(0, T_max);                 // generation time pdf
  
  // Calculate p vector -- the generation time probability distribution
  for (j in 1:T_max) {
          p[j] = gamma_cdf(j, shape, 1/scale) - gamma_cdf(j-1, shape, 1/scale);  
  }

  //normalize the generation time pdf      
  p_sum = sum(p[1:T_max]);

  for (j in 1:T_max) {
    p[j] = p[j] / p_sum;
  }
  
  // Calculate expected number of cases on each day (mu) 
  for (t in 1:T_max) {
    mu_tmp = 0.0;
    for (j in 1:t) {
      if (t!=j) {
	mu_tmp = N_mat[t-j]*p[j] + mu_tmp;
      } else {
	mu_tmp = N0*p[j] + mu_tmp; // If t==j, we use N0 
      }
    }
    
    mu[t] = mu_tmp*exp(logR); // Multiply mu_tmp vector R to get the expected number
    
  }
        
  // likelihood of incidence vector, given vector of mu
  N_mat[1:T_max] ~ poisson(mu);
  
}


