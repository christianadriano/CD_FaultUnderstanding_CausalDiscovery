" 

Bayesian Linear Regression experiments with the package Rethinking from McElreath

Fit models using MAP - Maximum a posteriori 

https://github.com/rmcelreath/rethinking

The strong assumption made by MAP is that the area close to the peak of the 
posterior distribution has a Gaussian distribution shape. 

The nice two aspects of the Gaussian distribution are that:
- we can easily define with only two parameters (mean and variance)
- we know how to compute the first derivative, which we can use to find the peak (where the derivative is zero)

The name quadratic approximation stems from the fact that the log of the Gaussian distribution is a parabola, 
which is a quadratic function. Hence, MAP represents any log-posterior with a parabola.

The process has two basic steps:
- Find the posterior mode (the peak)
- After finding the peak, compute the size of the curvature at the peak
With this we can compute the entire shape of the distribution.

"

#Example using precis function
library(rethinking)

#Quadratic Approximation (Maximum a posteriori)

#---------------------------
#Example-1 Water and Land area in the Globe
globe.qa <- rethinking::map(
  alist(
      W ~ dbinom( W+L ,p) , # binomial likelihood
      p ~ dunif(0,1) # uniform prior
    ) ,
  data=list(W=6,L=3) #has seen 6 water and 3 land
  ) 

#display resutls
precis( globe.qa )
#   Mean   StdDev 5.5% 94.5%
# p 0.67   0.16   0.42  0.92


#---------------------------
#Example-2 Height

data("Howell1")
height_data <- Howell1

#Make estimates considering flat prior.
precis(height_data,prob=0.95) #creates a table of estimates and standard errors.

#Build a prior for the mean (mu) and the standard deviation (sigma)
sample_mu <- rnorm(1e4, 178, 20)
sample_sigma <- runif(1e4, 0, 50)
prior_h <- rnorm(1e4, sample_mu, sample_sigma) 
dens(prior_h) #plots the prior


#Prepare list with functions
flist_height <- alist(
  height ~ dnorm( mu , sigma ) ,
  mu ~ dnorm( 178 , 20 ) ,
  sigma ~ dunif( 0 , 50 )
)

#Run the approximations
fit <- rethinking::map(
  flist=flist_height,
  data=height_data,
  debug=TRUE
  )

precis(fit)
#         Mean    StdDev   5.5%  94.5%
#   mu    138.40   1.18  136.52 140.29
# sigma  27.58     0.84   26.24  28.91

#I can also sample from the posterior...
post <- extract.samples(fit)
plot(post)

#---------------------------
#Example-3 Cars
data(cars)
flist <- alist(
  dist ~ dnorm( mean=mu , sd=sigma ) ,
  mu <- a+b*speed ,
  c(a,b) ~ dnorm(0,10) , 
  sigma ~ dcauchy(0,1)
)
fit <- map( flist , start=list(a=40,b=0.1,sigma=20) , data=cars , debug=TRUE)

#----------------------------------------------------------------------------------------------
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


#Should I standardize my predictor variables? Maybe for the large values like Duration and Size yes.

