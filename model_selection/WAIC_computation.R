"
Computing the WAIC for a model
Code based on McElreath's book - Statistical Rethinking
"
#UPDATING packages
remove.packages("rstan")
if (file.exists(".RData")) file.remove(".RData")

install.packages(c("coda","mvtnorm","devtools","loo"))
library(devtools)
devtools::install_github("rmcelreath/rethinking")
library(rethinking)

data(cars)

"1. Uses the MAP algorithm to compute the posterior"
m <- map(
  alist(
    dist ~ dnorm(mu,sigma),
    mu <- a + b*speed,
    a ~ dnorm(0,100),
    b ~ dnorm(0,10),
    sigma ~ dunif(1) #dexp
  ) , data=cars )
set.seed(94)

"2. Collects samples from the fit model m"
post <- extract.samples(m,n=1000)

"3. Computes the log-likelihood of each observation i at each sample s from the posterior"
n_samples <- 1000
logprob <- sapply( 1:n_samples ,
                   function(s) {
                     mu <- post$a[s] + post$b[s]*cars$speed
                     dnorm( cars$dist , mu , post$sigma[s] , log=TRUE )
                   },
                  simplify="array")

"4. compute the lppd"
n_cases <- nrow(cars)
lppd <- sapply( 1:n_cases , function(i) log_sum_exp(logprob[i,]) - log(n_samples) )
