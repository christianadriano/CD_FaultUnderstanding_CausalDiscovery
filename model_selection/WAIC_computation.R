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
#-----------------------------

#For execution on a local, multicore CPU with excess RAM
options(mc.cores = parallel::detectCores())

data(cars)

"1. Uses the MAP algorithm to compute the posterior"
m <- map(
  alist(
    dist ~ dnorm(mu,sigma),
    mu <- a + b*speed,
    a ~ dnorm(0,100),
    b ~ dnorm(0,10),
    sigma ~ dexp(1) #dexp
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
sum(lppd)
#[1] -206.9311

"5. compute penalties for each observation"
pWAIC <- sapply( 1:n_cases , function(i) var(logprob[i,]) )

"6. compute WAIC"
-2*( sum(lppd) - sum(pWAIC) )
#[1] 422.2855

"7. compute WAIC by observation"
waic_vec <- -2*( lppd - pWAIC )

"8. compute the standard error"
sqrt( n_cases*var(waic_vec) )
#[1] 17.35175

"9. Compare with WAIC function results"
WAIC(m)
# Constructing posterior predictions
# [ 1000 / 1000 ]
# [1] 423.0785
# attr(,"lppd")
# [1] -206.9319
# attr(,"pWAIC")
# [1] 4.607323
# attr(,"se")
# [1] 17.61842
  

#---------------
#Comparing models
m1.1 <- map(
  alist(
    dist ~ dnorm(mu,sigma),
    mu <- a + b*speed +c*speed^2,
    a ~ dnorm(0,100),
    b ~ dnorm(0,10),
    c ~ dnorm(0,10),
    sigma ~ dexp(1) #dexp
  ) , data=cars )

compare(m,m1.1)
#       WAIC  pWAIC dWAIC weight    SE  dSE
# m    423.0   4.6   0.0   0.58  17.70   NA
# m1.1 423.7   6.1   0.7   0.42  18.94 4.35


