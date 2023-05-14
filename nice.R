#Nice function
source("function_reposit.R")
set.seed(1234)

#Simulate data,
dat <- gen_lots_data(nreps = 5,nSubjs = 200, sdErr = 10, 
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
                     coef2Continuous = list(c(int2 = 1.0, slope2 = -3.0),
                                                       c(int2 = 0.0, slope2 = 3.0)))

#We need to scale the data now
for(i in seq_along(dat)){
  dat[[i]] <- dat[[i]] %>% mutate_at(c("X3", "X4"), ~(scale(.) %>% as.vector))
}

#Split into test and train
split <- tt_split(datasets = dat)

#Save this split data
save(split, file = "Split_data_Simulations")

#create list that STAN will use
test1 <- stan_data_loop(training_datasets = split$Training, testing_datasets = split$Testing)

#Compile STAN codes
mod <- stan_model("pred_error_uninform.stan")
#RUN STAN SAMPLER and extract output
test2 <- predfunct(stan_data_collection = test1)

#RMSE from results of test2
test4 <- rmse_function(test2, split$Testing)
test4 <- cleaning(test4)

#RIDGE Prior
mod1 <- stan_model("pred_error_ridge.stan")

testa <- predfunct(stan_data_collection = test1, stan_file = "pred_error_ridge.stan")
test5 <- rmse_function(testa, split$Testing)
test5 <- cleaning(test5)

#Local Student t 
mod2 <- stan_model("pred_error_studentt.stan")

testb <- predfunct(stan_data_collection = test1, stan_file = "pred_error_studentt.stan")
test6 <- rmse_function(testb, split$Testing)
test6 <- cleaning(test6)

#LASSO
mod3 <- stan_model("pred_error_lasso.stan")

testc <- predfunct(stan_data_collection = test1, stan_file = "pred_error_lasso.stan")
test7 <- rmse_function(testc, split$Testing)
test7 <- cleaning(test7)

#Elastic Net

#Hyperlasso
mod5 <- stan_model("pred_error_hyperlasso.stan")
teste <- predfunct(stan_data_collection = test1, stan_file = "pred_error_hyperlasso.stan")
test9 <- rmse_function(teste, split$Testing)
test9 <- cleaning(test9)

#Horseshoe
mod6 <- stan_model("pred_error_horseshoe.stan")
testf <- predfunct(stan_data_collection = test1, stan_file = "pred_error_horseshoe.stan")
test10 <- rmse_function(testf, split$Testing)
test10 <- cleaning(test10)

#Discrete Normal Mixture
mod8 <- stan_model("pred_error_bernoulli_mixture.stan")
testg <- predfunct(stan_data_collection = test1, stan_file = "pred_error_bernoulli_mixture.stan")
test11 <- rmse_function(testg, split$Testing)
test11 <- cleaning(test11)

#Uniform normal mixture
mod9 <- stan_model("pred_error_uniformmix.stan")
testh <- predfunct(stan_data_collection = test1, stan_file = "pred_error_uniformmix.stan")
test12 <- rmse_function(testh, split$Testing)
test12 <- cleaning(test12)



#Generate lots of data with Truncated Poisson distribution
playdoh2 <- gen_lots_data_trunc_Poisson(nreps = 5,nSubjs = 200, sdErr = 10, 
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
                                        coef2Continuous = list(c(int2 = 1.0, slope2 = -3.0),
                                                               c(int2 = 0.0, slope2 = 3.0)))

#We need to scale the data now
for(i in seq_along(playdoh2)){
  playdoh2[[i]] <- playdoh2[[i]] %>% mutate_at(c("X3", "X4"), ~(scale(.) %>% as.vector))
}

head(playdoh2[[1]])

#Split into test and train
split1 <- tt_split(datasets = playdoh2)

#Save this split data
save(split1, file = "Split_data_Simulations1")

#create list that STAN will use
test1a <- stan_data_loop(training_datasets = split1$Training, testing_datasets = split1$Testing)

#Compile STAN codes
mod <- stan_model("pred_error_uninform.stan")
test2a <- stan_out(stan_data_collection = test1a)
