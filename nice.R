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

#Compile STAN codes
mod <- stan_model("pred_error_uninform.stan")

#Data for STAN code
#extract level 2 variables
lev2_vars <- extract_lev2(split$Training[[1]], id, 1, cols_to_drop = c("id", "time", "Y", 
                                                                       "group", "id.new"))

#create list that STAN will use
test1 <- stan_data_loop(training_datasets = split$Training, testing_datasets = split$Testing)

#RUN STAN SAMPLER and extract output
test2 <- predfunct(stan_data_collection = test1)

#RMSE from results of test2
test4 <- rmse_function(test2, split$Testing)
test4 <- t(data.frame(test4))
row.names(test4) <- 1:nrow(test4)
colnames(test4) <- c("RMSE")
plot(test4)