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
}

parameters {
  
}

model {
  
}


generated quantities {
  //predictions
  vector[N_obs_test] y_new;
  for(n in 1:N_obs_test){
  }
}