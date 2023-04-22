#Trying brms
library(brms)

sim_brms <- brm(Y~time, data = mydat, chains = 3,
                iter = 3000, warmup = 1000)
summary(sim_brms)
plot(sim_brms)
pp_check(sim_brms)

#Adding random effects
bprior <- c(prior_string("horseshoe(1)", class = "b"))
set_prior(lasso(df = 1, scale = 10))
sim2_brms <- brm(Y~(X1 + X2 + X3 + X4)*time + (1+time|id), prior = set_prior(lasso(df = 1, scale = 10)),
                 data = mydat, chains = 3,
                 iter = 3000, warmup = 1000)
summary(sim2_brms)
plot(sim2_brms)
pp_check(sim2_brms)
sim2_brms$model


marginal_effects(sim2_brms)

loo(sim_brms,sim2_brms, compare = TRUE)