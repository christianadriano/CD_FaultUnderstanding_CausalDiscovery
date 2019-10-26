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
#-----------------------------

library(rethinking)

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
#[1] 423.3336

"7. compute WAIC by observation"
waic_vec <- -2*( lppd - pWAIC )

"8. compute the standard error"
sqrt( n_cases*var(waic_vec) )
#[1] 17.83451

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
# m    423.0   4.5   0.0   0.55  17.28   NA
# m1.1 423.7   5.9   0.4   0.45  18.69 4.35

#Small WAIC values are better (models are sorted by WAIC)

#Same comparision can be made using 
#Pareto-Smoothed Importance Sampling Cross-validaiton
compare(m,m1.1, func=LOO)
#      LOO pLOO dLOO weight   SE   dSE
# m    4.6  0.5  0.0    0.68  1.86   NA
# m1.1 6.4  0.6  1.5    0.32  2.29 0.71

"Going back to WAIC table, How much better is m compare to m1.1 ?
To answer this question, we need to look at the differences 
between the error. Take the difference in standard error (dse in 
m1.1 line). Let us first see how was this computed:"
dwaic_m <- WAIC(m, pointwise = TRUE)
dwaic_m1.1 <- WAIC(m1.1, pointwise = TRUE)
diff_m1.1_m <- dwaic_m - dwaic_m1.1

"Is this value large? We can check if this value causes 
the dWAIC to cross the zero. To do that, we can calculate the
a 99% interval difference, which has the corresponding z-score 2.6."

#interval_differnce:
dwaic <- 0.7 #from compare WAIC table
dse_m1.1 <- 4.35
z_score <- 2.6 #corresponding to 99% interval difference
dwaic + c(-1,1) * dse_m1.1 * z_score