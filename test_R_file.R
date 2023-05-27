source("function_storage.R")
source("function_reposit.R")
library(parallel) # to run parallel
rstan_options(auto_write = TRUE) # to avoid recompiling stan model
set.seed(1234)

num_datsets <- 5
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
