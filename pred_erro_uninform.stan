//prediction error Uninformative
data {
  #Training 
  int<lower=0> N_obs_train; // number of observations in training set
  int<lower=0> N_pts_train; // number of participants in training set
  
  int<lower=0> K;   // number of level 2 predictors
  int<lower=0> L;   // number of level 1 predictors
  
  vector[N_obs_train] y;      // response vector
  
  matrix[N_obs_train,L] x_train; // level 1 predictors
  matrix[N_pts_train, K] x2_train;   // level 2 predictor matrix

  int pid_train[N_obs_train]; // participant id vector.  Vector will identify each participant in dataset
  
  
  #Testing
  int<lower=0> N_obs_test; // number of observations in training set
  int<lower=0> N_pts_test; // number of participants in training set
  
  int pid_test[N_obs_test];
  
  matrix[N_obs_test,L] x_test; // level 1 predictors
  matrix[N_pts_test, K] x2_test;   // level 2 predictor matrix
}

parameters {
  real mu;
  real<lower=0> sigma2;
}