" 

Bayesian Linear Regression experiments with the package Rethinking from McElreath

Fit models using Maximum a posteriori 

https://github.com/rmcelreath/rethinking

"

#Example using precis function
library(rethinking)
data("Howell1")
height_data <- Howell1
precis(height_data,prob=0.95)
summary(result)
plot(result)

#Build a prior
sample_mu <- rnorm(1e4, 178, 20)
sample_sigma <- runif(1e4, 0, 50)
prior_h <- rnorm(1e4, sample_mu, sample_sigma)
dens(prior_h)

#Quadratic Approximation (Maximum A posterior)
flist <- alist(
  height ~ rnorm(mu, sigma),
  mu ~ rnorm(178, 20),
  sigma ~ runif(0, 50)
)

y_height=list(height_data$height)

fit <- map(
  flist,
  data=y_height,
  start=list(mu=0,sigma=0)
)

y_height=list(height_data$height)

fit <- map(
  flist,
  data=y_height,
  start=list(mu=0,sigma=0)
)


#Linear Regression example with MAP

xbar <- mean(height_data$weight)

flist <- alist(
  height ~ dnorm(mean=mu, sd=sigma),
  mu <- a + b*(weight - xbar),
  a ~ dnorm(178, 20),
  b ~ dlnorm(0,1), #use log-normal, because we know that height and weight are always positive
  sigma ~ dunif(0, 50)
)

fit <- rethinking::map(
  flist,
  start=list(a=1,b=0.1,sigma=25),
  data=height_data
)

#visualizing uncertainty by samplig from the posterior distribution
post <- extract.samples(fit)
fit[1:5, ]
#plot it

#Show confidence intervals
#Uses link function to generate multiple values of the posterior by given a set of inputs

post <- extract.samples(fit)
mu.link <-  function(weight) post$a + post$b*(weight - xbar)

#for each sample of the posterior compute the predicted value



