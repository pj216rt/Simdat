//prediction error Uninformative

data {
  int<lower=0> N_obs_train; // number of observations in training set
  int<lower=0> N_pts_train; // number of participants in training set
  
  int<lower=0> K;   // number of level 2 predictors
  int<lower=0> L;   // number of level 1 predictors
  
  int pid[N_obs]; // participant id vector.  Vector will identify each participant in dataset
  
  matrix[N_obs,L] x; // level 1 predictors
  matrix[N_pts, K] x2;   // level 2 predictor matrix
  
  vector[N_obs] y;      // outcome vector
}