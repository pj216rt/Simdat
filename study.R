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
elanet <- stan_model("pred_error_elasticnet.stan")

#Run simulation
nworkers <- detectCores() # number of cores to use
cl <- makePSOCKcluster(nworkers) # create cluster

#Aplying the source file for all cores in this paralell setting
clusterCall(cl, function() { source("function_reposit.R")})

out <- clusterApplyLB(cl, 1:nrow(conditions), simulate.bunches, cond=conditions, reps = num_datsets) # run simulation
stopCluster(cl) #shut down the nodes

#Testing function on just one condition for now
simulate.bunches(pos = 1, cond = conditions, reps = 1)

my_simulation_func(pos=1, cond = conditions)

