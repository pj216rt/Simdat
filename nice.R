#Nice function
source("function_reposit.R")
set.seed(1234)

#Simulate data,
dat <- gen_lots_data(nreps = 4,nSubjs = 200, sdErr = 10, 
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
split$Training[[2]]

#Compile STAN codes
mod <- stan_model("pred_error_uninform.stan")

#Data for STAN code
#extract level 2 variables
lev2_vars <- extract_lev2(split$Training[[1]], id, 1, cols_to_drop = c("id", "time", "Y", 
                                                                       "group", "id.new"))


for(i in seq_along(split$Training)){
  print("hello")
}

test <- multiple_extract_lev2_var(datasets = split)
split$Training[1]

###ERROR HERE ###
#create list that STAN will use
stan_dat <- list(
  N_obs_train = nrow(split$Training[[1]]),
  N_pts_train = n_distinct(split$Training[[1]]$id.new),
  L = 2, K = ncol(lev2_vars)+1,
  pid_train = split$Training[[1]]$id.new,
  x_train = cbind(1, split$Training[[1]]$time),
  x2_train = cbind(1, lev2_vars),
  y_train = split$Training[[1]]$Y,
  N_obs_test = nrow(split$Testing[[1]]),
  test_data = model.matrix(~(X1+X2+X3+X4)*time, data = split$Testing[[1]])
)

test1 <- stan_data_loop(datasets = split$Training, train_data = split$Training)
predfunct(test1)

#STAN SAMPLING
stan_fit <- stan(file = "pred_error_uninform.stan", data = stan_dat, iter = 2000, chains = 1)


#Extract the values from this STAN sampling
class(stan_fit)
ext_fit <- rstan::extract(stan_fit)
print(names(ext_fit))

pred_vals <- ext_fit$y_new

df_of_draw <- as.data.frame(stan_fit)
df_of_draw$`y_new[168]`
plot(df_of_draw$`y_new[168]`)
mean(df_of_draw$`y_new[168]`)
median(df_of_draw$`y_new[168]`)

split$Testing[[1]]$Y[[168]]
