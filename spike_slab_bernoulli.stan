//spike and slab prior
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
  matrix[L, N_pts] beta_p;
  vector<lower=0>[L] tau;      // prior scale
  matrix[K,L] gamma; //level 2 coefficients
  corr_matrix[L] Omega; // correlation matrix
  real<lower=0> sigma; // population sigma
  
  vector<lower=0>[K] tau2; //variance slab
}

transformed parameters {
  matrix [N_pts, L] beta;
  beta = x2*gamma;
}

model {
  vector[N_obs] mu;
  to_vector(gamma) ~ (bernoulli(0.5)*normal(0, tau2)) + ((1-bernoulli(0.5))*normal(0,0.001));
  
  tau2 ~ inv_gamma(0.5, 0.5);
  Omega ~ lkj_corr(1);
  tau ~ inv_gamma(1,7);
  sigma ~ inv_gamma(1,7);
  
  {
    matrix[L,L] Sigma_beta;
    Sigma_beta = quad_form_diag(Omega, tau);
    for (j in 1:N_pts){
      beta_p[, j] ~ multi_normal(beta[j], Sigma_beta);
    }
  }
  for(i in 1:N_obs) {
    mu[i] = (x[i] * (beta_p[, pid[i]])); // * is matrix multiplication in this context
  }
  
  y ~ normal(mu, sigma);
}
