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