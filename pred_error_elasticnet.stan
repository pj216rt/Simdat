//pred error elastic net
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
  matrix[N_obs_test,K*L] test_data; 
}

parameters {
  matrix[L, N_pts_train] beta_p;
  vector<lower=0>[L] tau;      // prior scale
  matrix[K,L] gamma; //level 2 coefficients
  corr_matrix[L] Omega; // correlation matrix
  real<lower=0> sigma; // population sigma
  
  //hyperparameters prior
	vector<lower=1>[K] taup;
  real<lower=0> lambda1; //penalty parameter
  real<lower=0> lambda2; //second penalty parameter
}

transformed parameters {
  real<lower=0> tau2; //prior variance
  matrix [N_pts_train, L] beta;
  beta = x2_train*gamma;
  
  tau2 = sigma^2/lambda;
}