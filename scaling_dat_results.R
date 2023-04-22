source("function_reposit.R")
set.seed(1234)

#Simulate data, unless overridden, defaults from function file are used
dat <- gen_lots_data(nreps = 4,coef2Continuous = list(c(int2 = 1.0, slope2 = -3.0),
                                            c(int2 = 0.0, slope2 = 3.0)))


#We need to scale the data now
for(i in dat){
  i <- i %>% mutate_at(c("X1", "X2", "X3", "X4", "Y"), ~(scale(.) %>% as.vector))
}

scaled.dat <- mydat %>% mutate_at(c("X1", "X2", "X3", "X4", "Y"), ~(scale(.) %>% as.vector))
lev2_var <- extract_lev2(scaled.dat, id, 1, cols_to_drop = c("id", "time", "Y"))
stan_dat <- list(N_obs = nrow(scaled.dat),
                 N_pts = max(as.numeric(scaled.dat$id)),
                 L = 2, K = ncol(lev2_var)+1,
                 pid = scaled.dat$id, x = cbind(1, scaled.dat$time),
                 x2 = cbind(1, lev2_var), y = scaled.dat$Y)

#Consistency of original model
mod <- stan_model("test2b.stan")
test1 <- data.frame(stan.consistency(bunches))

test1$index <- 1:nrow(test1)
d1 <- melt(test1, id.vars="index")
#Plot values of fixed effects for the nrep repetitions
p1 <- ggplot(d1, aes(index,value, col=variable)) + stat_smooth(se = F) +
  ggtitle("Coefficient Value over time, STAN") +
  xlab("Run Number") + ylab("Coefficient Value")

#Consistency of changed model
mod1 <- stan_model("test2c.stan")
test2 <- data.frame(stan.consistency(bunches))

test2$index <- 1:nrow(test2)
d2 <- melt(test2, id.vars="index")

#Plot values of fixed effects for the nrep repetitions
p2 <- ggplot(d2, aes(index,value, col=variable)) + stat_smooth(se = F) +
  ggtitle("Coefficient Value over time, STAN, New Model") +
  xlab("Run Number") + ylab("Coefficient Value")

#Plot comparing the two methods
grid.arrange(p1,p2)