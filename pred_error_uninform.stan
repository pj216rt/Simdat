//prediction error Uninformative
data {
  //Two sets of data training and testing
  //Training
  int<lower=0> N_obs_train; // number of observations
  int<lower=0> N_pts_train; // number of participants
  
  int<lower=0> K;   // number of level 2 predictors
  int<lower=0> L;   // number of level 1 predictors
  
  int pid_train[N_obs_train]; // participant id vector.  Vector will identify each participant in dataset
  
  matrix[N_obs_train,L] x_train; // level 1 predictors
  matrix[N_pts_train, K] x2_train;   // level 2 predictor matrix
  
  vector[N_obs_train] y_train;      // outcome vector
  
  //testing
  int<lower=0> N_obs_test; // number of observations
  int<lower=0> N_pts_test; // number of participants
  
  int pid_test[N_obs_test];
}

parameters {
  matrix[L, N_pts_train] beta_p;
  vector<lower=0>[L] tau;      // prior scale
  matrix[K,L] gamma; //level 2 coefficients
  corr_matrix[L] Omega; // correlation matrix
  real<lower=0> sigma; // population sigma
}

transformed parameters {
  matrix [N_pts_train, L] beta;
  beta = x2_test*gamma;
}

model {
  vector[N_obs] mu;
  to_vector(gamma) ~ normal(0,100);
  
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