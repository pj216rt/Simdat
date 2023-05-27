#Generate data used for simulation study
source("function_reposit.R")
library(parallel) # to run parallel


num_datsets <- 2

#Condition 1, level 2 coefficients of 4,2,0,0,3,0,0,0
#Generating data
#240 subjects
sim1 <- gen_balanced_datasets(nreps = num_datsets, nSubjs = 240, num_obs = 5, sdErr = 10, 
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
for(i in seq_along(sim1)){
  sim1[[i]] <- sim1[[i]] %>% mutate_at(c("X3", "X4"), ~(scale(.) %>% as.vector))
}

#Split into test and train
split.sim1 <- tt_split(datasets = sim1, percent_train = 0.80)
split.sim1 <- stan_data_loop(training_datasets = split.sim1$Training, testing_datasets = split.sim1$Testing)

#Save this condition
save(split.sim1, file = "simdata_sim1.RData")

#Condition 2, level 2 coefficients of 2,2,2,2,2,2,2,2
#240 subjects
sim2 <- gen_balanced_datasets(nreps = num_datsets, nSubjs = 240, num_obs = 5, sdErr = 10, 
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
                              coef2Binary = list(c(int2 = 2.0, slope2 = 2.0),
                                                 c(int2 = 2.0, slope2 = 2.0)),
                              coef2Continuous = list(c(int2 = 2.0, slope2 = 2.0),
                                                     c(int2 = 2.0, slope2 = 2.0)))

#We need to scale the data now
for(i in seq_along(sim2)){
  sim2[[i]] <- sim2[[i]] %>% mutate_at(c("X3", "X4"), ~(scale(.) %>% as.vector))
}

#Split into test and train
split.sim2 <- tt_split(datasets = sim2, percent_train = 0.80)
split.sim2 <- stan_data_loop(training_datasets = split.sim2$Training, testing_datasets = split.sim2$Testing)

#Save this condition
save(split.sim2, file = "simdata_sim2.RData")

#Condition 3, 15 coefficients of 3, 15 of 0, need to find how to duplicate values