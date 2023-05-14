#Fixed Number of visits
source("function_reposit.R")
set.seed(1234)

sample_sizes <- c(20, 50, 100, 200, 500)

#Simulate data, small dimensionality, increasing in sample size from 20 to 500
#20
dat <- gen_balanced_datasets(nreps = 5, nSubjs = 20, num_obs = 5, sdErr = 10, 
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
