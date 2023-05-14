#Fixed Number of visits
source("function_reposit.R")
set.seed(1234)

sample_sizes <- c(20, 50, 100, 200, 500)

#Simulate data, small dimensionality, increasing in sample size from 20 to 500
#20
dat <- genData_balanced()
