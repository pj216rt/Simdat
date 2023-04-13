library(lme4)
library(dplyr)

source("function_reposit.R")
set.seed(1234)

mydat <- mydat <- genData(nSubjs = 100, sdErr = 10, 
                          # intercept and slope fixed effects
                          coef1 = c(4, 3),
                          # types of level 2 covariates
                          level2Binary = c(.3, .7),
                          level2Continuous = list(c(mu = 0, sd = 1),
                                                  c(mu = 5, sd = 1)),
                          # corr between errors on subject-specific int and slope
                          corrRE = 0.20,
                          # sd of errors on subject-specific int and slope
                          sdRE = c(1, 1),
                          # for each predictor in level 2, (int2, slope2) 
                          # specify effect on level 2 intercept and level 2 slope
                          coef2Binary = list(c(int2 = 2.0, slope2 = 1.0),
                                             c(int2 = 4.0, slope2 = 3.0)),
                          coef2Continuous = list(c(int2 = 1.0, slope2 = -3.0),
                                                 c(int2 = 3.0, slope2 = 3.0))
)
#Plotting Data
xyplot(Y ~ time, data = mydat, type = "b", groups = id, 
       xlab="Time",
       ylab="Response Variable",
       main="Simulated Data: Response Variable vs. Time")

#Generate lots of data
bunches <- gen_lots_data(nreps = 100, nSubjs = 100, sdErr = 10, 
                         # intercept and slope fixed effects
                         coef1 = c(4, 3),
                         # types of level 2 covariates
                         level2Binary = c(.3, .7),
                         level2Continuous = list(c(mu = 0, sd = 1),
                                                 c(mu = 5, sd = 1)),
                         # corr between errors on subject-specific int and slope
                         corrRE = 0.20,
                         # sd of errors on subject-specific int and slope
                         sdRE = c(1, 1),
                         # for each predictor in level 2, (int2, slope2) 
                         # specify effect on level 2 intercept and level 2 slope
                         coef2Binary = list(c(int2 = 2.0, slope2 = 1.0),
                                            c(int2 = 4.0, slope2 = 3.0)),
                         coef2Continuous = list(c(int2 = 1.0, slope2 = -3.0),
                                                c(int2 = 3.0, slope2 = 3.0)))
length(bunches)

#Checking to see if running lmer many times reveals convergence of fixed effects
test <- data.frame(consistency_check(bunches))
test$index <- 1:nrow(test)
d <- melt(test, id.vars="index")

#Plot values of fixed effects for the nrep repetitions
ggplot(d, aes(index,value, col=variable)) + stat_smooth(se = F) 

#Put this is a loop
model <- stan_model("test2b.stan")
test1 <- stan.consistency(bunches, stan_file = "test2b.stan")
