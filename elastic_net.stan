//elastic net
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
  
  matrix[K,L] gamma_raw; //level 2 coefficients
  //hyperparameters prior
	vector<lower=1>[K] tau2;
	real<lower=0> lambda1;
	real<lower=0> lambda2;
}

transformed parameters {
  matrix [N_pts, L] beta;
  beta = x2*gamma;
}

model {
  vector[N_obs] mu;
  to_vector(gamma) ~ normal(0, 1); //implies beta ~ normal(0, sqrt(1/(lambda2/sigma2 * (tau[j]/(tau[j]-1)))))
  
  tau ~ gamma(0.5, (8*lambda2*sigma)/(lambda1^2));
	lambda1 ~ cauchy(0, 1);
	lambda2 ~ cauchy(0, 1);
	
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
