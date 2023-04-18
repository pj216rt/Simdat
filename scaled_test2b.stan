//Standardized STAN MODEL

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

transformed data {
  matrix[N_obs,L] x_std; // level 1 predictors
  matrix[N_pts, K] x2_std;   // level 2 predictor matrix
  
  vector[N_obs] y_std;      // outcome vector
}