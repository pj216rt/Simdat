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

#Balanced dataset generation, but with an option to pass a list of sample sizes as an argument
gen_balanced_datasets_long <- function(nreps = 10, nSubjs = list_sampsize, num_obs = 5, sdErr = 10, 
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
  for(i in seq_along(nSubjs)){
    temp <- list()
    for(j in 1:nreps){
      temp[[j]] <- genData_balanced(nSubjs[[i]], num_obs, sdErr, coef1, level2Binary, 
                                    level2Continuous, corrRE, sdRE,coef2Binary,coef2Continuous)
      simdata[[i]] <- temp
    }
  }
  simdata <- unlist(simdata, recursive = FALSE)
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
  #Creating formula to plug into the test data model matrix later
  names_of_lev2_vars <- colnames(level2_vars[[1]])
  test_dat_lev2_vars <- paste0(names_of_lev2_vars, collapse = "+") #get level 2 variables and combine them into a formula for later
  test_dat_lev2_vars <- paste0("Y~(",test_dat_lev2_vars, ")", "*time")
  test_dat_form <- as.formula(paste0(test_dat_lev2_vars))
  print(test_dat_lev2_vars)
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
      test_data = model.matrix(test_dat_form, data = testing_datasets[[i]])
    )
    stan_dat[[i]] <- temp
  }
  #print(stan_dat[[1]])
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
    simdata <- standat[1:reps]
  }
  else if(condition==2){
    standat <- split.sim2
    simdata <- standat[1:reps]
  }
  #Add more conditions as they are added
  
  #Compile data
  temp1 <- paste0("pred_error_", prior, ".stan")
  modFB <- stan_model(temp1)
  
  #Run STAN sampler
  #Can change this the number of iterations and chains
  counter <- 0 # initialize counter to keep track
  out <- lapply(simdata, function(x){
    counter <<- counter + 1
    print(paste("replication", counter))
    
    #Running the STAN Sampler
    fit.stan <- stan_out(stan_data_collection = standat, stan_file = temp1)
    
    for(i in fit.stan){
      t <- typeof(i)
      
      out <- summary(i)$summary
      rhat <- out[which(out[, "Rhat"] > 1.1), "Rhat"] # PSR > 1.1
      sp <- get_sampler_params(i, inc_warmup=F)
      div <- sapply(sp, function(x) sum(x[, "divergent__"])) # divergent transitions
      
      ### Extract output ###
      pars <- i@model_pars
      ## posterior estimates regression coefficients and hyperparameters ##
      pars.sel <- pars[-grep("y_new", pars)] # remove linear predictor from output
      
      fit.summary <- summary(i, probs=seq(0, 1, 0.05))$summary # extract summary
      post.mean <- fit.summary[-grep("y_new", rownames(fit.summary)), "mean"]
      #print(post.mean)
      post.median <- fit.summary[-grep("y_new", rownames(fit.summary)), "50%"]
      #print(post.median)
      #extract posterior draws from the second half of each chain (excluding burn-in)
      post.draws <- rstan::extract(i, pars=pars.sel)
      
      # estimate posterior modes based on the posterior density
      estimate_mode <- function(draws){
        d <- density(draws)
        d$x[which.max(d$y)]
      }
      post.mode <- lapply(post.draws, function(x){
        if(length(dim(x)) == 1){estimate_mode(x)}
        else(apply(x, 2, estimate_mode))
      })
      
      ## credible intervals ##
      ci <- fit.summary[-grep("y_new", rownames(fit.summary)), grep("%", colnames(fit.summary))]
      
      ## posterior standard deviations ##
      post.sd <- fit.summary[, "sd"]
      
      ## variable selection based on scaled neighborhood criterion ##
      sd.inter <- cbind(-post.sd[grep("gamma\\b", names(post.sd))], post.sd[grep("gamma\\b", names(post.sd))])
      draws.gamma <- post.draws[[grep("gamma\\b", names(post.draws))]]
      dim(draws.gamma) <- c(1000,10)
      #print(draws.gamma[,,2])
      post.prob <- rep(NA, nrow(sd.inter))
      for(i in 1:nrow(sd.inter)){ # compute the posterior probability in [-post.sd, post.sd]
        post.prob[i] <- sum(sd.inter[i, 1] <= draws.gamma[,i] & sd.inter[i, 2] >= draws.gamma[,i])/nrow(draws.gamma)
      }
      excl.pred.snc <- matrix(NA, nrow=11, ncol=length(post.prob)) # matrix with TRUE if predictor is not zero
      colnames(excl.pred.snc) <- rownames(sd.inter)
      rownames(excl.pred.snc) <- c("prob0", "prob0.1", "prob0.2", "prob0.3", "prob0.4", 
                                   "prob0.5", "prob0.6", "prob0.7", "prob0.8", "prob0.9", "prob1")
      for(i in 1:length(post.prob)){
        sq <- seq(0, 1, 0.1)
        excl.pred.snc[, i] <- sapply(sq, function(x) post.prob[i] <= x) 
      }
      
      ## generated y values test set ##
      ygen <- fit.summary[grep("y_new", rownames(fit.summary)), "mean"]
      
      ### Return output ###
      out <- list("Rhat > 1.1"=rhat, "Number of divergent transitions"=div, "Posterior means"=post.mean, 
                  "Posterior medians"=post.median, "Posterior modes"=post.mode, 
                  "Credible intervals"=ci,"Posterior standard deviations"=post.sd, 
                  "Excluded predictors based on scaled neighborhood criterion"=excl.pred.snc, 
                  "Generated y-values test data"=ygen)
      
      return(out)
    }
  })
}
