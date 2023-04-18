//test 2c
data {
  int<lower=0> N_obs; // number of observations
  int<lower=0> N_pts; // number of participants
  
  int<lower=0> K;   // number of level 2 predictors
  int<lower=0> L;   // number of level 1 predictors
  
  int pid[N_obs]; // participant id vector.  Vector will identify each participant in dataset
  
  matrix[N_obs,L] x; // level 1 predictors
  matrix[N_pts, K] x2;   // level 2 predictor matrix
  
  vector[N_obs] y;      // outcome vector
}

parameters {
  //vector[L] beta_p [N_pts];  // ind participant int, slope by individual
  matrix[N_pts, L] beta_p;
  vector<lower=0>[L] tau;      // prior scale
  matrix[K,L] gamma; //level 2 coefficients
  vector[L] beta;
  corr_matrix[L] Omega; // correlation matrix
  real<lower=0> sigma; // population sigma
}

transformed parameters {
  matrix [N_pts, L] lev2;
  lev2 = x2*gamma;
}

model {
  vector[N_obs] mu;
  to_vector(gamma) ~ normal(0,100);
  Omega ~ lkj_corr(1);
  tau ~ inv_gamma(1,7);
  sigma ~ inv_gamma(1,7);
  
  for (j in 1:N_pts){
    beta_p[j, ] ~ multi_normal(beta, quad_form_diag(Omega, tau));
  }
  
  for(i in 1:N_obs) {
    mu[i] = (beta_p[pid[i], 1] + lev2[pid[i], 1]) + (beta_p[pid[i], 2] + lev2[pid[i], 2])*x[i,2];
    // )
  }
  y ~ normal(mu, sigma);
}
