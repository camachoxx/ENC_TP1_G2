# Exercise 3

# Last edited: 09/10.2019


g <- function(x){sqrt(-log(x))/2}
# (a) Use the R function integrate() to compute the value of I
I <- integrate(g, lower = 0, upper = 1)
I.value

# (b) Describe and implement in R the Monte Carlo method of size m = 10000 for estimating 
# Ihat. Report an estimate of the variance of the Monte Carlo estimator I_MChatof I.

set.seed(456) 
m=10000 
x=runif(m,0,1)
I_MC <- mean(g(x))
error <- abs(I_MC - I$value) 
I_MC_var <- var(g(x))/m # estimate of the variance
