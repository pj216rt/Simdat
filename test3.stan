//test3
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
  matrix[L, N_pts] z;
  matrix[L, N_pts] beta_p;
  matrix[K,L] gamma; //level 2 coefficients
  real<lower=0> sigma; // population sigma
  
  cholesky_factor_corr[L] L_Omega;
  vector<lower=0,upper=pi()/2>[L] tau_unif;
}

transformed parameters {
  vector<lower=0>[L] tau;      // prior scale
  for (l in 1:L){
    tau[L] = 2.5 * tan(tau_unif[L]);
  }
  matrix [N_pts, L] beta;
  //beta = x2*gamma;
  beta = x2*gamma + (diag_pre_multiply(tau,L_Omega) * z)';
}

model {
  vector[N_obs] mu;
  to_vector(gamma) ~ normal(0,100);
  to_vector(z) ~ std_normal();
  L_Omega ~ lkj_corr_cholesky(1);
  
  //Omega ~ lkj_corr(1);
  tau ~ inv_gamma(1,7);
  sigma ~ inv_gamma(1,7);
  
  {
    matrix[L,L] Sigma_beta;
    Sigma_beta = quad_form_diag(L_Omega, tau);
    for (j in 1:N_pts){
      beta_p[, j] ~ multi_normal(beta[j], Sigma_beta);
    }
  }
  for(i in 1:N_obs) {
    mu[i] = (x[i] * (beta_p[, pid[i]])); // * is matrix multiplication in this context
  }
  
  y ~ normal(mu, sigma);
}
