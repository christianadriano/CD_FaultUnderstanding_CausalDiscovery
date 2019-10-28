library(rethinking)

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
prior <- extract.prior( m5.1 )
A_seq <- seq( from=-2 , to=2 , length.out=50 )
mu <- link( m5.1 , post=prior , data=list( A=A_seq ) )
plot( NULL , xlim=c(-2,2) , ylim=c(-2,2),
      xlab="Median Marriage rate (std)",
      ylab="Divorce Rate (std)",)
title("Prior Simulation")

for ( i in 1:50 ) lines(A_seq, mu[i,] , col=col.alpha("black",0.4))
#lwd=0.4)

#col=col.alpha("black",0.4))


Ax_seq <- seq(from=0,to=0, length.out = 50)

mA <- cbind(Ax_seq,A_seq)
colnames(mA) <- c("x","y")
mA_df <- data.frame(mA)

#for ( i in 1:50 ) 
  lines( mA_df , mu_dff, col=col.alpha("black",0.4)  )


m5.2 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bA * A ,
    a ~ dnorm( 0 , 0.2 ) ,
    bA ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = d )

par(mfrow=c(2,2))
dev.off()

precis(m5.1)
precis(m1_2ndOrder)
plot(coeftab(m1_NoInter,m1_2ndOrder), par=c("a","by"),
     xlab="Estimate")
