#clean function repository
require(MBESS)
require(mnormt)
require(lattice)
require(nlme)
require(ggplot2)
require(reshape2)
require(rstan)
require(dplyr)
require(tidyverse)
library(bayesplot) # to save traceplots

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

#Need to tweak this
stan_data_loop <- function(training_datasets, testing_datasets){
  level2_vars <- multiple_extract_lev2_var(training_datasets)
  names_of_lev2_vars <- colnames(level2_vars[[1]])
  test_dat_lev2_vars <- paste0(names_of_lev2_vars, collapse = "+") #get level 2 variables and combine them into a formula for later
  print(test_dat_lev2_vars)
  test_dat_lev2_vars <- eval(test_dat_lev2_vars)
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
      test_data = model.matrix(~(test_dat_lev2_vars)*time, data = testing_datasets[[i]])
    )
    stan_dat[[i]] <- temp
  }
  #print(stan_dat[[1]])
  return(stan_dat)
}
