source("function_storage.R")
source("function_reposit.R")
library(parallel) # to run parallel
rstan_options(auto_write = TRUE) # to avoid recompiling stan model
set.seed(1234)

num_datsets <- 2
sample_size <- c(50,100,500)
dat <- gen_balanced_datasets_long(nreps = num_datsets, nSubjs = sample_size, num_obs = 5, sdErr = 10, 
                             # intercept and slope fixed effects
                             coef1 = c(4, 3),
                             # types of level 2 covariates
                             level2Binary = c(0.2, 0.4),
                             level2Continuous = list(c(mu = 0, sd = 1),
                                                     c(mu = 5, sd = 1)),
                             # corr between errors on subject-specific int and slope
                             corrRE = 0.50,
                             # sd of errors on subject-specific int and slope
                             sdRE = c(1, 1),
                             # for each predictor in level 2, (int2, slope2) 
                             # specify effect on level 2 intercept and level 2 slope
                             coef2Binary = list(c(int2 = 4.0, slope2 = 3.0),
                                                c(int2 = 2.0, slope2 = 0.0)),
                             coef2Continuous = list(c(int2 = 0.0, slope2 = 0.0),
                                                    c(int2 = 0.0, slope2 = 0.0)))
#We need to scale the data now
#only scale continuous data
for(i in seq_along(dat)){
  dat[[i]] <- dat[[i]] %>% mutate_at(c("X3", "X4"), ~(scale(.) %>% as.vector))
}

#Split into test and train
split.sim1 <- tt_split(datasets = dat, percent_train = 0.80)
split.sim1 <- stan_data_loop(training_datasets = split.sim1$Training, testing_datasets = split.sim1$Testing)



#compile stan models
comp <- stan_model("pred_error_uninform.stan")

#Running the STAN Sampler
fit.stan <- stan_out(stan_data_collection = split.sim1)

#Get output from this stan output
for(i in fit.stan){
  out <- summary(i)$summary
  rhat <- out[which(out[, "Rhat"] > 1.1), "Rhat"] # PSR > 1.1
  sp <- get_sampler_params(i, inc_warmup=F)
  div <- sapply(sp, function(x) sum(x[, "divergent__"])) # divergent transitions
  
  ### Extract output ###
  pars <- i@model_pars
  ## posterior estimates regression coefficients and hyperparameters ##
  pars.sel <- pars[-grep("y_new", pars)] # remove linear predictor from output
  
  fit.summary <- summary(i, pars=pars.sel, probs=seq(0, 1, 0.05))$summary # extract summary
  post.mean <- fit.summary[, "mean"]
  #print(post.mean)
  post.median <- fit.summary[, "50%"]
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
}
