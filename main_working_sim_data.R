library(lme4)
library(gridExtra)


source("function_reposit.R")
set.seed(1234)

mydat <- genData(nSubjs = 200, sdErr = 10, 
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
                                                 c(int2 = 0.0, slope2 = 3.0))
)
#Plotting Data
xyplot(Y ~ time, data = mydat, type = "b", groups = id, 
       xlab="Time",
       ylab="Response Variable",
       main="Simulated Data: Response Variable vs. Time")

#Examine covariance structure of lmer
me1 <- lmer(Y ~ (X1 + X2 + X3 + X4)*time + (1+time||id), data = mydat)
summary(me1)
m <- vcov(me1)
vcov(me1) |> cov2cor()


#Generate lots of data
bunches <- gen_lots_data(nreps = 100, nSubjs = 200, sdErr = 10, 
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
p <- ggplot(d, aes(index,value, col=variable)) + stat_smooth(se = F) +
  ggtitle("Coefficient Value over time, LMER") +
  xlab("Run Number") + ylab("Coefficient Value")
p
#Put this is a loop

mod <- stan_model("test2b.stan")
test1 <- data.frame(stan.consistency(bunches))

test1$index <- 1:nrow(test1)
d1 <- melt(test1, id.vars="index")

#Plot values of fixed effects for the nrep repetitions
p1 <- ggplot(d1, aes(index,value, col=variable)) + stat_smooth(se = F) +
  ggtitle("Coefficient Value over time, STAN") +
  xlab("Run Number") + ylab("Coefficient Value")

#Plot comparing the two methods
grid.arrange(p,p1)




library(splmm)

#need to fit a model.matrix to the data
x <- model.matrix(Y ~ (X1+X2+X3)*time, mydat)
z <- x
fit <- splmm(x=x,y=mydat$Y,z=z,grp=mydat$id,lam1=0.01,
             lam2=0.01,penalty.b="lasso", penalty.L="lasso")
summary(fit)
fit$coefficients
fit$stopped
plot(fit$residuals)

#loop over the bunches collection of data
test2 <- data.frame(splm_loop(bunches))
test2$index <- 1:nrow(test2)
d2 <- melt(test2, id.vars="index")

p1 <- ggplot(d2, aes(index,value, col=variable)) + stat_smooth(se = F) +
  ggtitle("Coefficient Value over time, SPLMM") +
  xlab("Run Number") + ylab("Coefficient Value")
p1



mod3 <- stan_model("test2c.stan")
lev2_var <- extract_lev2(mydat, id, 1, cols_to_drop = c("id", "time", "Y"))
stan_dat <- list(N_obs = nrow(mydat),
                 N_pts = max(as.numeric(mydat$id)),
                 L = 2, K = ncol(lev2_var),
                 pid = mydat$id, x = cbind(1, mydat$time),
                 x2 = lev2_var, y = mydat$Y)

stan_fit <- stan(file = "test2c.stan", data = stan_dat, iter = 3000, chains = 1)
gamma_summary <- summary(stan_fit, pars = c("beta"), probs = c(0.1, 0.9))$summary
print(gamma_summary)


#LASSO
mod4 <- stan_model("lasso_test.stan")
stan_fit1 <- stan(file = "lasso_test.stan", data = stan_dat, iter = 2000, chains = 1)
gamma_summary <- summary(stan_fit1, pars = c("gamma"), probs = c(0.1, 0.9))$summary
print(gamma_summary)



test <- stan_model("lasso_test.stan")
test1 <- stan_model("ridge_test.stan")

stan_fit1 <- stan(file = "test2c.stan", data = stan_dat, iter = 3000, chains = 1)
gamma_summary <- summary(stan_fit1, pars = c("gamma"), probs = c(0.1, 0.9))$summary
print(gamma_summary)
coef_var_stan(gamma_summary)


stan_fit <- stan(file = "test2b.stan", data = stan_dat, iter = 6000, chains = 1)
gamma_summary <- summary(stan_fit, pars = c("gamma"), probs = c(0.1, 0.9))$summary
print(gamma_summary)

SNR_stan(gamma_summary)

me1 <- lmer(Y ~ (X1 + X2 + X3 + X4)*time + (1+time|id), data = scaled.dat)
summary(me1)
