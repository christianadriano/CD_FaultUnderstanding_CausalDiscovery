" 

Bayesian Linear Regression experiments with the package Rethinking from McElreath

https://github.com/rmcelreath/rethinking

"

#Example using precis function
library(rethinking)
data("Howell1")
d <- Howell1
precis(d,prob=0.95)
summary(result)
plot(result)

#Build a prior
sample_mu <- rnorm(1e4, 178, 20)
sample_sigma <- runif(1e4, 0, 50)
prior_h <- rnorm(1e4, sample_mu, sample_sigma)
dens(prior_h)
