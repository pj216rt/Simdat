source("function_reposit.R")
set.seed(1234)

#Simulate data, unless overridden, defaults from function file are used
dat <- gen_lots_data(nreps = 31,coef2Continuous = list(c(int2 = 1.0, slope2 = -3.0),
                                            c(int2 = 0.0, slope2 = 3.0)))

#We need to scale the data now
for(i in dat){
  i <- i %>% mutate_at(c("X3", "X4"), ~(scale(.) %>% as.vector))
}

#Consistency of original model
mod <- stan_model("test2b.stan")
test1 <- data.frame(stan.consistency(dat))

test1$index <- 1:nrow(test1)
d1 <- melt(test1, id.vars="index")
#Plot values of fixed effects for the nrep repetitions
p1 <- ggplot(d1, aes(index,value, col=variable)) + stat_smooth(se = F) +
  ggtitle("Coefficient Value over time, STAN") +
  xlab("Run Number") + ylab("Coefficient Value")
p1

#Consistency of changed model
mod1 <- stan_model("test2c.stan")
test2 <- data.frame(stan.consistency(dat))

test2$index <- 1:nrow(test2)
d2 <- melt(test2, id.vars="index")

#Plot values of fixed effects for the nrep repetitions
p2 <- ggplot(d2, aes(index,value, col=variable)) + stat_smooth(se = F) +
  ggtitle("Coefficient Value over time, STAN, New Model") +
  xlab("Run Number") + ylab("Coefficient Value")
p2
#Plot comparing the two methods
grid.arrange(p1,p2)


ot <- SNR_loop(dat)
