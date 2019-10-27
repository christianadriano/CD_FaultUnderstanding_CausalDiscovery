"
Computing the WAIC for a model
Code based on McElreath's book - Statistical Rethinking
"
#UPDATING packages
remove.packages("rstan")
remove.packages("rethinking")
if (file.exists(".RData")) file.remove(".RData")

install.packages(c("coda","mvtnorm","devtools","loo"))
library(loo)
library(mvtnorm)
library(devtools)
devtools::install_github("rmcelreath/rethinking", ref="Experimental")
#-----------------------------

library(rethinking)
library(ggplot2)

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
    mu <- b*speed,
    b ~ dnorm(0,10),
    sigma ~ dexp(1) #dexp
  ) , data=cars )

m1.2 <- map(
  alist(
    dist ~ dnorm(mu,sigma),
    mu <- a,
    a ~ dnorm(0,10),
    sigma ~ dexp(1) #dexp
  ) , data=cars )

compare(m,m1.1,m1.2)
#       WAIC  pWAIC dWAIC weight    SE   dSE
# m    423.8   5.0   0.0   0.87  17.85    NA
# m1.1 427.6   3.6   3.8   0.13  17.07  5.58
# m1.2 475.9   3.1  52.2   0.00  18.21 15.00

plot(compare(m,m1.1,m1.2))

#Small WAIC values are better (models are sorted by WAIC)

#Same comparision can be made using 
#Pareto-Smoothed Importance Sampling Cross-validaiton
compare(m,m1.1,m1.2, func=LOO)
#      LOO pLOO dLOO weight   SE   dSE
# m    4.6  0.5  0.0    0.68  1.86   NA
# m1.1 6.4  0.6  1.5    0.32  2.29 0.71

#      LOO pLOO dLOO weight   SE  dSE
# m1.2 3.0  0.4  0.0   0.48 1.29   NA
# m1.1 4.0  0.4  1.1   0.28 1.96 0.83
# m    4.4  0.4  1.4   0.24 1.63 0.84

"Going back to WAIC table, How much better is m compare to m1.1 ?
To answer this question, we need to look at the differences 
between the error. Take the difference in standard error (dse in 
m1.1 line). Let us first see how was this computed:"
set.seed(85)
waic_m <- WAIC(m, pointwise = TRUE)
waic_m1.1 <- WAIC(m1.1, pointwise = TRUE)
diff_m_m1.1 <- waic_m - waic_m1.1
sum(diff_m_m1.1)
# 0.4000746

"Is this value large? We can check if this value causes 
the dWAIC to cross the zero. To do that, we can calculate the
a 99% interval difference, which has the corresponding z-score 2.6."

#interval_differnce:
dwaic <- 0.7 #from compare WAIC table
dse_m1.1 <- 4.35
z_score <- 2.6 #corresponding to 99% interval difference
dwaic + c(-1,1) * dse_m1.1 * z_score
#[1] -10.61  12.01, we can see that it crosses the zero, hence the models 
#are not very easy to distinguish.

table_df <- data.frame(table@output)

table <- compare(m,m1.1)
plot()

qplot(models,WAIC, data=table_df)+
  geom_errorbar(aes(x=models, ymin=WAIC-dWAIC, ymax=WAIC+dWAIC), width=0.25)

