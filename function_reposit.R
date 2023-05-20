require(MBESS)
require(mnormt)
require(lattice)
require(nlme)
require(ggplot2)
require(reshape2)
require(rstan)
require(dplyr)
require(tidyverse)

genData <- function(nSubjs = 100, sdErr = 1, 
                    # intercept and slope fixed effects
                    coef1 = c(4, 3),
                    # types of level 2 covariates
                    level2Binary = c(.2, 0.7),
                    level2Continuous = list(c(mu = 0, sd = 1),
                                            c(mu = 5, sd = 1)),
                    # corr between errors on subject-specific int and slope
                    corrRE = 0.20,
                    # sd of errors on subject-specific int and slope
                    sdRE = c(1, 1),
                    # for each predictor in level 2, (int2, slope2) 
                    # specify effect on level 2 intercept and level 2 slope
                    coef2Binary = list(c(int2 = 2.0, slope2 = 1.0),
                                       c(int2 = 1.0, slope2 = 3.0)),
                    coef2Continuous = list(c(int2 = 1.0, slope2 = 1.5),
                                           c(int2 = 0.2, slope2 = 3.0))
){
  # subject IDs
  ids <- 1:nSubjs
  
  # number of observation times for each subject
  ntimes <- rpois(n = nSubjs, lambda = 3) + 1
  # subject observation times (stacked)
  subjTimes <- sequence(ntimes, from = 0, by = 1)
  
  # reshape ids for long format
  ids <- rep(ids, ntimes)
  
  # generate level 2 predictor variables
  # binary predictors
  nBinaryVars <- length(level2Binary)
  B <- rbinom(nSubjs*nBinaryVars, size = 1, 
              prob = rep(level2Binary, each = nSubjs))
  Bmat <- matrix(B, ncol = nBinaryVars)
  # continuous predictors
  nContVars <- length(level2Continuous)
  means <- sapply(level2Continuous, function(x) x["mu"])
  sds <- sapply(level2Continuous, function(x) x["sd"])
  Z <- rnorm(nSubjs*nContVars, 
             mean = rep(means, each = nSubjs),
             sd = rep(sds, each = nSubjs))
  Zmat <- matrix(Z, ncol = nContVars)
  # all level 2 predictors
  Xmat <- cbind(Bmat, Zmat)
  colnames(Xmat) <- paste0("X", 1:(nBinaryVars + nContVars))
  
  # var-cov of the random effects at level 2
  covRE <- cor2cov(matrix(c(1, corrRE, corrRE, 1), nrow = 2), sdRE)
  cat("Population corr matrix of level 2 errors is:\n")
  print(cov2cor(covRE))
  
  # random errors at level 2: columns are Intercept, Slope
  REs <- rmnorm(nSubjs, mean = rep(0, 2), varcov = covRE)
  cat("Sample corr matrix of level 2 errors is:\n")
  print(cor(REs), digits = 3)
  
  # subject-specific (level 2) intercept and slope
  allcoefs <- matrix(c(unlist(coef2Binary), unlist(coef2Continuous)), nrow = 2)
  #print(allcoef)
  rownames(allcoefs) <- c("int2", "slope2")
  coef2int <- allcoefs[1,]
  coef2slope <- allcoefs[2,]
  REintslope <- Xmat %*% cbind(coef2int, coef2slope)
  REcoefs <- REintslope + REs
  # reshape to allow for longitudinal data
  REcoefs <- matrix(rep(REcoefs, rep(ntimes, 2)), ncol = 2)
  
  # iid error for the response
  respError <- rnorm(sum(ntimes), 0, sd = sdErr)
  
  Y <- (coef1[1] + REcoefs[,1]) +
    (coef1[2] + REcoefs[,2]) * subjTimes + respError
  
  # construct long format data frame
  covars <- matrix(rep(Xmat, rep(ntimes, nBinaryVars+nContVars)), ncol = ncol(Xmat))
  colnames(covars) <- colnames(Xmat)
  ans <- cbind(id = ids, time = subjTimes, covars, Y)
  ans <- data.frame(ans)
  
  attributes(ans) <- c(attributes(ans),
                       list(coef1 = coef1, coef2 = allcoefs,
                            REcoefs = REcoefs,
                            sdErr = sdErr, corrRE = corrRE, sdRE = sdRE
                       )
  )
  ans
}

###Want to generate data where every subject is observed a constant number of times
#Error Here somewhere
genData_balanced <- function(nSubjs = 100, num_obs = 5, sdErr = 10, 
                    # intercept and slope fixed effects
                    coef1 = c(4, 3),
                    # types of level 2 covariates
                    level2Binary = c(.2, 0.7),
                    level2Continuous = list(c(mu = 0, sd = 1),
                                            c(mu = 5, sd = 1)),
                    # corr between errors on subject-specific int and slope
                    corrRE = 0.20,
                    # sd of errors on subject-specific int and slope
                    sdRE = c(1, 1),
                    # for each predictor in level 2, (int2, slope2) 
                    # specify effect on level 2 intercept and level 2 slope
                    coef2Binary = list(c(int2 = 2.0, slope2 = 1.0),
                                       c(int2 = 1.0, slope2 = 3.0)),
                    coef2Continuous = list(c(int2 = 1.0, slope2 = 1.5),
                                           c(int2 = 0.2, slope2 = 3.0))
){
  # subject IDs
  ids <- 1:nSubjs
  
  # number of observation times for each subject
  ntimes <- replicate(n=nSubjs, num_obs)
  # subject observation times (stacked)
  subjTimes <- sequence(ntimes, from = 0, by = 1)
  
  # reshape ids for long format
  ids <- rep(ids, ntimes)
  
  # generate level 2 predictor variables
  # binary predictors
  nBinaryVars <- length(level2Binary)
  B <- rbinom(nSubjs*nBinaryVars, size = 1, 
              prob = rep(level2Binary, each = nSubjs))
  Bmat <- matrix(B, ncol = nBinaryVars)
  # continuous predictors
  nContVars <- length(level2Continuous)
  means <- sapply(level2Continuous, function(x) x["mu"])
  sds <- sapply(level2Continuous, function(x) x["sd"])
  Z <- rnorm(nSubjs*nContVars, 
             mean = rep(means, each = nSubjs),
             sd = rep(sds, each = nSubjs))
  Zmat <- matrix(Z, ncol = nContVars)
  # all level 2 predictors
  Xmat <- cbind(Bmat, Zmat)
  colnames(Xmat) <- paste0("X", 1:(nBinaryVars + nContVars))
  
  # var-cov of the random effects at level 2
  covRE <- cor2cov(matrix(c(1, corrRE, corrRE, 1), nrow = 2), sdRE)
  cat("Population corr matrix of level 2 errors is:\n")
  print(cov2cor(covRE))
  
  # random errors at level 2: columns are Intercept, Slope
  REs <- rmnorm(nSubjs, mean = rep(0, 2), varcov = covRE)
  cat("Sample corr matrix of level 2 errors is:\n")
  print(cor(REs), digits = 3)
  
  # subject-specific (level 2) intercept and slope
  allcoefs <- matrix(c(unlist(coef2Binary), unlist(coef2Continuous)), nrow = 2)
  rownames(allcoefs) <- c("int2", "slope2")
  coef2int <- allcoefs[1,]
  coef2slope <- allcoefs[2,]
  REintslope <- Xmat %*% cbind(coef2int, coef2slope)
  REcoefs <- REintslope + REs
  # reshape to allow for longitudinal data
  REcoefs <- matrix(rep(REcoefs, rep(ntimes, 2)), ncol = 2)
  
  # iid error for the response
  respError <- rnorm(sum(ntimes), 0, sd = sdErr)
  
  Y <- (coef1[1] + REcoefs[,1]) +
    (coef1[2] + REcoefs[,2]) * subjTimes + respError
  
  # construct long format data frame
  covars <- matrix(rep(Xmat, rep(ntimes, nBinaryVars+nContVars)), ncol = ncol(Xmat))
  colnames(covars) <- colnames(Xmat)
  ans <- cbind(id = ids, time = subjTimes, covars, Y)
  ans <- data.frame(ans)
  
  attributes(ans) <- c(attributes(ans),
                       list(coef1 = coef1, coef2 = allcoefs,
                            REcoefs = REcoefs,
                            sdErr = sdErr, corrRE = corrRE, sdRE = sdRE
                       )
  )
  ans
}

#Use a truncated Poisson distribution for number of visits by each subject?
genData_truncPoisson <- function(nSubjs = 100, sdErr = 10, 
                             # intercept and slope fixed effects
                             coef1 = c(4, 3),
                             # types of level 2 covariates
                             level2Binary = c(.2, 0.7),
                             level2Continuous = list(c(mu = 0, sd = 1),
                                                     c(mu = 5, sd = 1)),
                             # corr between errors on subject-specific int and slope
                             corrRE = 0.20,
                             # sd of errors on subject-specific int and slope
                             sdRE = c(1, 1),
                             # for each predictor in level 2, (int2, slope2) 
                             # specify effect on level 2 intercept and level 2 slope
                             coef2Binary = list(c(int2 = 2.0, slope2 = 1.0),
                                                c(int2 = 1.0, slope2 = 3.0)),
                             coef2Continuous = list(c(int2 = 1.0, slope2 = 1.5),
                                                    c(int2 = 0.2, slope2 = 3.0))
){
  # subject IDs
  ids <- 1:nSubjs
  
  # number of observation times for each subject
  ntimes <- rtpois(n = nSubjs, lambda = 3, a = 5)       #Lower limit of 5?
  # subject observation times (stacked)
  subjTimes <- sequence(ntimes, from = 0, by = 1)
  
  # reshape ids for long format
  ids <- rep(ids, ntimes)
  
  # generate level 2 predictor variables
  # binary predictors
  nBinaryVars <- length(level2Binary)
  B <- rbinom(nSubjs*nBinaryVars, size = 1, 
              prob = rep(level2Binary, each = nSubjs))
  Bmat <- matrix(B, ncol = nBinaryVars)
  # continuous predictors
  nContVars <- length(level2Continuous)
  means <- sapply(level2Continuous, function(x) x["mu"])
  sds <- sapply(level2Continuous, function(x) x["sd"])
  Z <- rnorm(nSubjs*nContVars, 
             mean = rep(means, each = nSubjs),
             sd = rep(sds, each = nSubjs))
  Zmat <- matrix(Z, ncol = nContVars)
  # all level 2 predictors
  Xmat <- cbind(Bmat, Zmat)
  colnames(Xmat) <- paste0("X", 1:(nBinaryVars + nContVars))
  
  # var-cov of the random effects at level 2
  covRE <- cor2cov(matrix(c(1, corrRE, corrRE, 1), nrow = 2), sdRE)
  cat("Population corr matrix of level 2 errors is:\n")
  print(cov2cor(covRE))
  
  # random errors at level 2: columns are Intercept, Slope
  REs <- rmnorm(nSubjs, mean = rep(0, 2), varcov = covRE)
  cat("Sample corr matrix of level 2 errors is:\n")
  print(cor(REs), digits = 3)
  
  # subject-specific (level 2) intercept and slope
  allcoefs <- matrix(c(unlist(coef2Binary), unlist(coef2Continuous)), nrow = 2)
  rownames(allcoefs) <- c("int2", "slope2")
  coef2int <- allcoefs[1,]
  coef2slope <- allcoefs[2,]
  REintslope <- Xmat %*% cbind(coef2int, coef2slope)
  REcoefs <- REintslope + REs
  # reshape to allow for longitudinal data
  REcoefs <- matrix(rep(REcoefs, rep(ntimes, 2)), ncol = 2)
  
  # iid error for the response
  respError <- rnorm(sum(ntimes), 0, sd = sdErr)
  
  Y <- (coef1[1] + REcoefs[,1]) +
    (coef1[2] + REcoefs[,2]) * subjTimes + respError
  
  # construct long format data frame
  covars <- matrix(rep(Xmat, rep(ntimes, nBinaryVars+nContVars)), ncol = ncol(Xmat))
  colnames(covars) <- colnames(Xmat)
  ans <- cbind(id = ids, time = subjTimes, covars, Y)
  ans <- data.frame(ans)
  
  attributes(ans) <- c(attributes(ans),
                       list(coef1 = coef1, coef2 = allcoefs,
                            REcoefs = REcoefs,
                            sdErr = sdErr, corrRE = corrRE, sdRE = sdRE
                       )
  )
  ans
}


#Generate data sets
gen_lots_data <- function(nreps = 10,
                          nSubjs = 100, sdErr = 1, 
                          # intercept and slope fixed effects
                          coef1 = c(4, 3),
                          # types of level 2 covariates
                          level2Binary = c(.2, 0.7),
                          level2Continuous = list(c(mu = 0, sd = 1),
                                                  c(mu = 5, sd = 1)),
                          # corr between errors on subject-specific int and slope
                          corrRE = 0.20,
                          # sd of errors on subject-specific int and slope
                          sdRE = c(1, 1),
                          # for each predictor in level 2, (int2, slope2) 
                          # specify effect on level 2 intercept and level 2 slope
                          coef2Binary = list(c(int2 = 2.0, slope2 = 1.0),
                                             c(int2 = 1.0, slope2 = 3.0)),
                          coef2Continuous = list(c(int2 = 1.0, slope2 = 1.5),
                                                 c(int2 = 0.2, slope2 = 3.0))){
  #generate nreps datasets
  simdata <- list()
  for(i in 1:nreps){
    simdata[[i]] <- genData(nSubjs, sdErr, coef1, level2Binary, level2Continuous, corrRE, sdRE,
                            coef2Binary,coef2Continuous)
  }
  return(simdata)
}

#Balanced datasets
gen_balanced_datasets <- function(nreps = 10, nSubjs = 100, num_obs = 5, sdErr = 10, 
                                  # intercept and slope fixed effects
                                  coef1 = c(4, 3),
                                  # types of level 2 covariates
                                  level2Binary = c(.2, 0.7),
                                  level2Continuous = list(c(mu = 0, sd = 1),
                                                          c(mu = 5, sd = 1)),
                                  # corr between errors on subject-specific int and slope
                                  corrRE = 0.20,
                                  # sd of errors on subject-specific int and slope
                                  sdRE = c(1, 1),
                                  # for each predictor in level 2, (int2, slope2) 
                                  # specify effect on level 2 intercept and level 2 slope
                                  coef2Binary = list(c(int2 = 2.0, slope2 = 1.0),
                                                     c(int2 = 1.0, slope2 = 3.0)),
                                  coef2Continuous = list(c(int2 = 1.0, slope2 = 1.5),
                                                         c(int2 = 0.2, slope2 = 3.0))){
  
  #generate nreps datasets
  simdata <- list()
  for(i in 1:nreps){
    simdata[[i]] <- genData_balanced(nSubjs, num_obs, sdErr, coef1, level2Binary, 
                                     level2Continuous, corrRE, sdRE,coef2Binary,coef2Continuous)
  }
  return(simdata)
}

#Generate lots of data with truncated Poisson for visits
gen_lots_data_trunc_Poisson <- function(nreps = 10,
                          nSubjs = 100, sdErr = 1, 
                          # intercept and slope fixed effects
                          coef1 = c(4, 3),
                          # types of level 2 covariates
                          level2Binary = c(.2, 0.7),
                          level2Continuous = list(c(mu = 0, sd = 1),
                                                  c(mu = 5, sd = 1)),
                          # corr between errors on subject-specific int and slope
                          corrRE = 0.20,
                          # sd of errors on subject-specific int and slope
                          sdRE = c(1, 1),
                          # for each predictor in level 2, (int2, slope2) 
                          # specify effect on level 2 intercept and level 2 slope
                          coef2Binary = list(c(int2 = 2.0, slope2 = 1.0),
                                             c(int2 = 1.0, slope2 = 3.0)),
                          coef2Continuous = list(c(int2 = 1.0, slope2 = 1.5),
                                                 c(int2 = 0.2, slope2 = 3.0))){
  #generate nreps datasets
  simdata <- list()
  for(i in 1:nreps){
    simdata[[i]] <- genData_truncPoisson(nSubjs, sdErr, coef1, level2Binary, level2Continuous, corrRE, sdRE,
                            coef2Binary,coef2Continuous)
  }
  return(simdata)
}

#Split data into test and train
tt_split <- function(datasets, var_to_select = id ,percent_train = 0.80){
  train_dat <- list()
  test_dat <- list()
  for(i in seq_along(datasets)){
    data <- datasets[[i]]
    
    #Essentially create a new column that specifies what group each participant is in
    groups <- data %>% select(id) %>% distinct(id) %>% rowwise() %>%
      mutate(group = sample(
        c("train", "test"), 1,
        replace = TRUE,
        prob = c(percent_train, (1-percent_train)) #weights for each group
      ))
    
    #left joined groups to the data
    data <- data %>% left_join(groups)
    
    #Split data into test and train
    #Train data
    train_dat[[i]] <- filter(data, group == "train")
    train_dat[[i]]$id.new <- match(train_dat[[i]]$id, unique(train_dat[[i]]$id))
    
    #Test data
    test_dat[[i]] <- filter(data, group == "test")
    test_dat[[i]]$id.new <- match(test_dat[[i]]$id, unique(test_dat[[i]]$id))
  }
  out <- list(Training = train_dat, Testing = test_dat)
  return(out)
}

extract_lev2 <- function(dat, id, filter_num, 
                         cols_to_drop = c("time", "subject", "group")){
  f <- dat %>% group_by(id) %>% filter(row_number()==filter_num)
  df <- f[, ! names(f) %in% cols_to_drop, drop = F]
  return(df)
}

#run lmer on dataset
consistency_check <- function(datasets){
  check <- list()
  for(i in seq_along(datasets)){
    test <- lmer(Y ~ (X1 + X2 + X3 + X4)*time + (1+time||id), data = datasets[[i]])
    temp <- fixef(test)
    check[[i]] <- temp
  }
  check <- t(data.frame(check))
  row.names(check) <- seq.int(nrow(check))
  return(check)
}

#Loop over STAN data to check for consistency
stan.consistency <- function(datasets, stan_file = "test2b.stan"){
  check <- list()
  for(i in seq_along(datasets)){
    lev2_vars <- extract_lev2(datasets[[i]], id, 1, cols_to_drop = c("id", "time", "Y"))
    
    stan_dat <- list(
      N_obs = nrow(datasets[[i]]),
      N_pts = max(as.numeric(datasets[[i]]$id)),
      L = 2, K = ncol(lev2_vars)+1,
      pid = datasets[[i]]$id,
      x = cbind(1, datasets[[i]]$time), 
      x2 = cbind(1, lev2_vars),
      y = datasets[[i]]$Y
    )
    stan_fit <- stan(file = "test2b.stan", data = stan_dat, iter = 2000, chains = 1)
    gamma_summary <- summary(stan_fit, pars = c("gamma"), probs = c(0.1, 0.9))$summary
    gamma_summary_mean <- gamma_summary[, c("mean")]
    print(gamma_summary)
    check[[i]] <- gamma_summary_mean
  }
  check <- t(data.frame(check))
  row.names(check) <- seq.int(nrow(check))
  return(check)
}


#Loop over SPLMM function to check for consistency
splm_loop <- function(datasets){
  check <- list()
  for(i in seq_along(datasets)){
    x <- model.matrix(Y ~ (X1+X2+X3)*time, datasets[[i]])
    z <- x
    fit <- splmm(x=x,y=datasets[[i]]$Y,z=z,grp=datasets[[i]]$id,lam1=0.01,
                 lam2=0.01,penalty.b="lasso", penalty.L="lasso")
    check[[i]] <- fit$coefficients
  }
  check <- t(data.frame(check))
  row.names(check) <- seq.int(nrow(check))
  return(check)
}

#Compute Coefficient of Variation for STAN results
coef_var_stan <- function(stan_ouput){
  temp <- data.frame(stan_ouput)
  temp["COEF_VAR"] = temp[3]/temp[1]
  return(temp)
}

#Compute SNR ratio
SNR_stan <- function(stan_output){
  temp <- data.frame(stan_output)
  temp["SNR"] = temp[1]/temp[3]
  return(temp)
}

#function to plot SNR for STAN
SNR_loop <- function(datasets, stan_file = "test2c.stan"){
  check <- list()
  for(i in seq_along(datasets)){
    lev2_vars <- extract_lev2(datasets[[i]], id, 1, cols_to_drop = c("id", "time", "Y"))
    
    stan_dat <- list(
      N_obs = nrow(datasets[[i]]),
      N_pts = max(as.numeric(datasets[[i]]$id)),
      L = 2, K = ncol(lev2_vars)+1,
      pid = datasets[[i]]$id,
      x = cbind(1, datasets[[i]]$time), 
      x2 = cbind(1, lev2_vars),
      y = datasets[[i]]$Y
    )
    stan_fit <- stan(file = "test2b.stan", data = stan_dat, iter = 2000, chains = 1)
    gamma_summary <- summary(stan_fit, pars = c("gamma"), probs = c(0.1, 0.9))$summary
    temp <- SNR_stan(gamma_summary)
    temp <- temp[, c("SNR")]
    print(temp)
    check[[i]] <- temp
  }
  check <- t(data.frame(check))
  row.names(check) <- seq.int(nrow(check))
  return(check)
}

#STAN output loop
stan_out <- function(datasets, stan_file = "test2b.stan"){
  #pre allocate vector
  check <- array()
  for(i in seq_along(datasets)){
    lev2_vars <- extract_lev2(datasets[[i]], id, 1, cols_to_drop = c("id", "time", "Y"))
    
    
    #Create data to feed into sampler
    stan_dat <- list(
      N_obs = nrow(datasets[[i]]),
      N_pts = max(as.numeric(datasets[[i]]$id)),
      L = 2, K = ncol(lev2_vars)+1,
      pid = datasets[[i]]$id,
      x = cbind(1, datasets[[i]]$time), 
      x2 = cbind(1, lev2_vars),
      y = datasets[[i]]$Y
    )
    
    #run STAN code
    stan_fit <- stan(file = "test2b.stan", data = stan_dat, iter = 2000, chains = 1)
    temp <- as.matrix(stan_fit)
    print(colnames(temp))
    check[i] <- append(check, temp)
  }
  return(check)
}

#Extract level 2 variables across multiple datasets
multiple_extract_lev2_var <- function(datasets, 
                                      cols_drop = c("id", "time", "Y", 
                                                    "group", "id.new")){
  output <- list()
  for(i in seq_along(datasets)){
    #print("hello")
    lev2_vars <- extract_lev2(datasets[[i]], id, 1, cols_to_drop=cols_drop)
    output[[i]] <- lev2_vars
  }
  return(output)
}

stan_data_loop <- function(training_datasets, testing_datasets){
  level2_vars <- multiple_extract_lev2_var(training_datasets)
  #print(level2_vars[[2]])
  stan_dat <- list()
  
  #Creating lists of data to feed into STAN sampler
  for(i in seq_along(training_datasets)){
    holder <- as.data.frame(level2_vars[[i]])
    temp <- list(
      N_obs_train = nrow(training_datasets[[i]]),
      N_pts_train = n_distinct(training_datasets[[i]]$id.new),
      L = 2, K = ncol(level2_vars[[i]])+1,
      pid_train = training_datasets[[i]]$id.new,
      x_train = cbind(1, training_datasets[[i]]$time),
      x2_train = cbind(1, holder),
      y_train = training_datasets[[i]]$Y,
      N_obs_test = nrow(testing_datasets[[i]]),
      test_data = model.matrix(~(X1+X2+X3+X4)*time, data = testing_datasets[[i]])
      )
    stan_dat[[i]] <- temp
  }
  #print(stan_dat[[1]])
  return(stan_dat)
}

#function to extract all of the STAN output
stan_out <- function(stan_data_collection, stan_file = "pred_error_uninform.stan", iterations = 2000,
                     chains_to_run = 1){
  output <- list()
  for(i in seq_along(stan_data_collection)){
    print("Hello")
    #print(stan_data_collection[[i]])
    stan_fit <- stan(file = stan_file, data = stan_data_collection[[i]], iter = iterations, 
                     chains = chains_to_run)
    df_of_draws <- stan_fit
    output[[i]] <- df_of_draws
  }
  return(output)
}

#Extract the output from the stan sampling function
stan_output_extract <- function(complete_stan_data, pars_to_consider = c("y_new"),
                                probabilities = c(0.1, 0.9)){
  output <- list()
  for(i in seq_along(complete_stan_data)){
    print("Hello")
    temp <- summary(complete_stan_data[[i]], pars = pars_to_consider, probs = probabilities)$summary
    output[[i]] <- temp
  }
  return(output)
}

#Prediction accuracy
#Do stan sampling for each of the repeated simulated data that we have
predfunct <- function(stan_data_collection, stan_file = "pred_error_uninform.stan", method = "mean"){
  output <- list()
  #stan_fit <- stan(file = stan_file, data = stan_data_collection[[1]], iter = 2000, chains = 1)
  for(i in seq_along(stan_data_collection)){
    print("Hello")
    #print(stan_data_collection[[i]])
    stan_fit <- stan(file = stan_file, data = stan_data_collection[[i]], iter = 2000, chains = 1)
    df_of_draws <- summary(stan_fit, pars = c("y_new"), probs = c(0.1, 0.9))$summary
    output[[i]] <- df_of_draws
  }
  return(output)
}

#Function to calculate RMSE from the predfunc, using the mean
rmse_function <- function(collection_of_data, actual_ydat){
  output <- list()
  for(i in seq_along(collection_of_data)){
    print("Hello")
    temp <- collection_of_data[[i]][, c("mean")]
    names(temp) <- NULL
    
    temp2 <- sqrt(mean((actual_ydat[[i]]$Y - temp)^2))
    output[[i]] <- temp2
  }
  return(output)
}

#Clean RMSE data
cleaning <- function(RMSEvals){
  temp <- t(data.frame(RMSEvals))
  row.names(temp) <- 1:nrow(temp)
  colnames(temp) <- c("RMSE")
  plot(temp)
}

stan_data_loop1 <- function(training_datasets, testing_datasets){
  level2_vars <- multiple_extract_lev2_var(training_datasets)
  #print(level2_vars[[2]])
  stan_dat <- list()
  
  #Creating lists of data to feed into STAN sampler
  for(i in seq_along(training_datasets)){
    holder <- as.data.frame(level2_vars[[i]])
    temp <- list(
      N_obs_train = nrow(training_datasets[[i]]),
      N_pts_train = n_distinct(training_datasets[[i]]$id.new),
      L = 2, K = ncol(level2_vars[[i]])+1,
      pid_train = training_datasets[[i]]$id.new,
      x_train = cbind(1, training_datasets[[i]]$time),
      x2_train = cbind(1, holder),
      y_train = training_datasets[[i]]$Y,
      N_obs_test = nrow(split$Testing[[i]]),
      test_data = model.matrix(~(X1+X2+X3+X4+X5+X6+X7+X8+X9+X10)*time, data = testing_datasets[[i]])
    )
    stan_dat[[i]] <- temp
  }
  print(stan_dat[[1]])
  return(stan_dat)
}


#Simulation study function
simulate.bunches <- function(pos, cond, reps){
  
  mypath <- file.path("G:/Simulations/")
  
  ###select condition
  prior <- cond$prior[pos]
  condition <- cond$condition[pos]
  
  #Search directory for simulation data corresponding to the condition
  temp <- paste0("simdata_sim", condition, ".RData") #Searching for RData file from simulation
  load(list.files()[grep(temp, list.files())]) #loading in the data.  
  
  #if loops for various conditions
  if(condition==1){ #If we are looking at condition 1, we want to use the split.sim1 data.
    standat <- split.sim1
    #Need to create list of data that the STAN sampler can use
  }
  else if(condition==2){
    standat <- split.sim2
  }
  #Add more conditions as they are added
  
  #Compile data
  temp1 <- paste0("pred_error_", prior, ".stan")
  modFB <- stan_model(temp1)
  
  #Run STAN sampler
  #Can change this the number of iterations and chains
  fit.stan <- stan_out(stan_data_collection = standat, stan_file = temp1)
  
  
  ###Convergence of STAN model
}

