#Simulation study
#Palmer Swanson
source("function_reposit.R")
library(parallel) # to run parallel
rstan_options(auto_write = TRUE) # to avoid recompiling stan model
set.seed(1234)

num_con <- 2

#Running stan codes for this condition
#list of priors
priors <- c("uninform")

cond <- 1:num_con

conditions <- expand.grid(prior = priors, condition=cond)

#compile stan models
comp <- stan_model("pred_error_uninform.stan")
comp1 <- stan_model("pred_error_lasso.stan")

#Run simulation
nworkers <- detectCores() # number of cores to use
cl <- makePSOCKcluster(nworkers) # create cluster
clusterCall(cl, function() library(rstan))
clusterCall(cl, function() library(bayesplot))
out <- clusterApplyLB(cl, 1:nrow(conditions), simulate.bunches, cond=conditions, reps = 5) # run simulation
stopCluster(cl) #shut down the nodes
