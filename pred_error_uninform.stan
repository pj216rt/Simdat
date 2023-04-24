//prediction error Uninformative
data {
  //Testing
  int<lower=0> N_obs_test; // number of observations in training set
  int<lower=0> N_pts_test; // number of participants in training set
  
  int<lower=0> K;   // number of level 2 predictors
  int<lower=0> L;   // number of level 1 predictors
  
  matrix[N_obs_test,L] x_test; // level 1 predictors
  matrix[N_pts_test, K] x2_test;   // level 2 predictor matrix
  
  int pid_test[N_obs_test]; // participant id vector.  Vector will identify each participant in dataset
  
  int N_samples;
  
  vector[N_samples] gamma;
}

parameters {
  
}

model {
  
}

generated quantities {
  //predictions
  matrix [N_pts_test, L] beta;
  beta = x2_test*gamma;
  
  {
    matrix[L,L] Sigma_beta;
    Sigma_beta = quad_form_diag(Omega, tau);
    for (j in 1:N_pts){
      beta_p[, j] ~ multi_normal(beta[j], Sigma_beta);
    }
  }
  
  matrix[N_samples, N_obs_test] y_test;
  for(n in 1:N_obs_test){
    for ( i in 1:N_samples){
      y_test[i,n] = normal_rng(x_test)
    }
  }
  
}