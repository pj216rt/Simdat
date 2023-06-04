library(extraDistr)


simdat <- function(num_subj, num_obs, type_o_data = c("balanced", "random", "truncated_Pois"), lam = 3,
                   lower_trunc = 4, upper_trunc = 10,
                   level2Binary = c(0.5),
                   level2Continuous = list(c(mu = 0, sd = 1),
                                           c(mu = 5, sd = 1))){
  # subject IDs
  ids <- 1:num_subj
  
  # number of observation times for each subject
  if(type_o_data == "balanced"){
    ntimes <- replicate(n=num_subj, num_obs)
  }
  else if(type_o_data == "random"){
    ntimes <- rpois(n = num_subj, lambda = lam) + 1
  }
  else if(type_o_data == "truncated_Pois"){
    ntimes <- rtpois(n=num_subj, lambda = lam, a=lower_trunc, b=upper_trunc)
  }
  
  # subject observation times (stacked)
  subjTimes <- sequence(ntimes, from = 0, by = 1)
  
  # reshape ids for long format
  ids <- rep(ids, ntimes)
  
  #generate level 2 predictor variables
  #Consider binary variables first
  nBinaryVars <- length(level2Binary)
  B <- rbinom(num_subj*nBinaryVars, size = 1, 
              prob = rep(level2Binary, each = num_subj))
  Bmat <- matrix(B, ncol = nBinaryVars)
  # continuous predictors
  nContVars <- length(level2Continuous)
  means <- sapply(level2Continuous, function(x) x["mu"])
  sds <- sapply(level2Continuous, function(x) x["sd"])
  Z <- rnorm(num_subj*nContVars, 
             mean = rep(means, each = num_subj),
             sd = rep(sds, each = num_subj))
  Zmat <- matrix(Z, ncol = nContVars)
  # all level 2 predictors
  Xmat <- cbind(Bmat, Zmat)
  colnames(Xmat) <- paste0("X", 1:(nBinaryVars + nContVars))
  print(Xmat)
  
  out <- cbind(ids, subjTimes)
  return(out)
}


out <- simdat(num_subj = 5, num_obs = 5, type_o_data = "balanced")
out

