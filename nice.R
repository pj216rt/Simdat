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
for(i in dat){
  i <- i %>% mutate_at(c("X3", "X4"), ~(scale(.) %>% as.vector))
}

#Split into test and train
split <- tt_split(datasets = dat)

#Save this split data
save(split, file = "Split_data_Simulations")

#Data for STAN code
#extract level 2 variables
#lev2_vars <- extract_lev2(split$Training[[1]], id, 1, cols_to_drop = c("id", "time", "Y", 
                                                                       #"group", "id.new"))
#create list that STAN will use
test1 <- stan_data_loop(training_datasets = split$Training, testing_datasets = split$Testing)

#Compile STAN codes
mod <- stan_model("pred_error_uninform.stan")
#RUN STAN SAMPLER and extract output
test2 <- predfunct(stan_data_collection = test1)

#RMSE from results of test2
test4 <- rmse_function(test2, split$Testing)
test4 <- t(data.frame(test4))
row.names(test4) <- 1:nrow(test4)
colnames(test4) <- c("RMSE")
plot(test4)

#RIDGE Prior
mod1 <- stan_model("pred_error_ridge.stan")

testa <- predfunct(stan_data_collection = test1, stan_file = "pred_error_ridge.stan")
test5 <- rmse_function(testa, split$Testing)
test5 <- t(data.frame(test5))
row.names(test5) <- 1:nrow(test5)
colnames(test5) <- c("RMSE")
plot(test5)

#Local Student t 
mod2 <- stan_model("pred_error_studentt.stan")

testb <- predfunct(stan_data_collection = test1, stan_file = "pred_error_studentt.stan")
test6 <- rmse_function(testb, split$Testing)
test6 <- t(data.frame(test6))
row.names(test6) <- 1:nrow(test6)
colnames(test6) <- c("RMSE")
plot(test6)

#LASSO
mod3 <- stan_model("pred_error_lasso.stan")

testc <- predfunct(stan_data_collection = test1, stan_file = "pred_error_lasso.stan")
test7 <- rmse_function(testc, split$Testing)
test7 <- t(data.frame(test7))
row.names(test7) <- 1:nrow(test7)
colnames(test7) <- c("RMSE")
plot(test7)

#Elastic Net

#Hyperlasso
mod5 <- stan_model("pred_error_hyperlasso.stan")
teste <- predfunct(stan_data_collection = test1, stan_file = "pred_error_hyperlasso.stan")
test9 <- rmse_function(teste, split$Testing)
test9 <- t(data.frame(test9))
row.names(test9) <- 1:nrow(test9)
colnames(test9) <- c("RMSE")
plot(test9)

#Horseshoe
mod6 <- stan_model("pred_error_horseshoe.stan")
testf <- predfunct(stan_data_collection = test1, stan_file = "pred_error_horseshoe.stan")
test10 <- rmse_function(testf, split$Testing)
test10 <- t(data.frame(test10))
row.names(test10) <- 1:nrow(test10)
colnames(test10) <- c("RMSE")
plot(test10)

#Discrete Normal Mixture
mod8 <- stan_model("pred_error_bernoulli_mixture.stan")
testg <- predfunct(stan_data_collection = test1, stan_file = "pred_error_bernoulli_mixture.stan")
test11 <- rmse_function(testg, split$Testing)
test11 <- t(data.frame(test11))
row.names(test11) <- 1:nrow(test11)
colnames(test11) <- c("RMSE")
plot(test11)

#Uniform normal mixture
mod9 <- stan_model("pred_error_uniformmix.stan")
testh <- predfunct(stan_data_collection = test1, stan_file = "pred_error_uniformmix.stan")
test12 <- rmse_function(testh, split$Testing)
test12 <- t(data.frame(test12))
row.names(test12) <- 1:nrow(test12)
colnames(test12) <- c("RMSE")
plot(test12)



#Test
playdoh <- stan_out(stan_data_collection = test1)
summary(playdoh[[1]], probs=c(0.1, 0.9))$summary
