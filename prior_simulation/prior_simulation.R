library(rethinking)

"Reset graphics card"
par(mfrow=c(2,2))
dev.off()

#Quick way to simulate priors for height (from example 4.14 McElreath)
sample_mu <- rnorm( 1e4 , 178 , 20 ) #sample from the mean height prior
sample_sigma <- runif( 1e4 , 0 , 50 ) #sample from the variance height prior
prior_h <- rnorm( 1e4 , sample_mu , sample_sigma ) #compute the joint probability
dens( prior_h ) #plot it.

"Note however, that priors are also posteriors, so we can also compute a
posterior and sample from it. That's what I will do next"

data(WaffleDivorce)
d <- WaffleDivorce
# standardize variables
d$A <- scale( d$MedianAgeMarriage )
d$D <- scale( d$Divorce )
m5.1 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bA * A ,
    a ~ dnorm( 0 , 0.2 ) ,
    bA ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = d )

set.seed(10)
prior <- extract.prior( m5.3 )
A_seq <- seq( from=-2 , to=2 , length.out=50 )
mu <- link( m5.3 , post=prior , data=list( A=A_seq ) )
plot( NULL , xlim=c(-2,2) , ylim=c(-2,2),
      xlab="Median Marriage rate (std)",
      ylab="Divorce Rate (std)",)
title("Prior Simulation")

for ( i in 1:50 ) lines(A_seq, mu[i,] , col=col.alpha("black",0.4))

m5.2 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bA * A ,
    a ~ dnorm( 0 , 0.2 ) ,
    bA ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = d )

m5.3 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bA * A ,
    a ~ dnorm( 0 , 0.2 ) ,
    bA ~ dlnorm( 0 , 0.5 ) , #forces a positive relationship with median age of marriage to be positive
    sigma ~ dexp( 1 )
  ) , data = d )


"Log-Normal priors are commonplace. They are an easy way to enforce positive relationships."


precis(m5.1)
plot(coeftab(m5.1,m5.2,m5.3), par=c("a","bA"),
     xlab="Estimate")
