" 
Compares models using Deviance (Log-Pointwise Predictive Density)

"



library(rethinking)
library(MASS)

load("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//model_selection//sim_train_test.R")

#Code 7.17
N <- 20
kseq <- 1:5
dev <- sapply( kseq , function(k) {
  print(k);
  r <- replicate( 1e4 , sim.train.test( N=N, k=k ) );
  c( mean(r[1,]) , mean(r[2,]) , sd(r[1,]) , sd(r[2,]) )
} )

#code 7.19
plot( 1:5 , dev[1,] , ylim=c( min(dev[1:2,])-5 , max(dev[1:2,])+10 ) ,
      xlim=c(1,5.1) , xlab="number of parameters" , ylab="deviance" ,
      pch=16 , col=rangi2 )
mtext( concat( "N = ",N ) )
points( (1:5)+0.1 , dev[2,] )
for ( i in kseq ) {
  pts_in <- dev[1,i] + c(-1,+1)*dev[3,i]
  pts_out <- dev[2,i] + c(-1,+1)*dev[4,i]
  lines( c(i,i) , pts_in , col=rangi2 )
  lines( c(i,i)+0.1 , pts_out )
}


---------------
  
  sim.train.test <- function( N=20 , k=3 , rho=c(0.15,-0.4) , b_sigma=100 , DIC=FALSE , WAIC=FALSE, devbar=FALSE , devbarout=FALSE ) {
    #require(MASS)
    n_dim <- 1+length(rho)
    if ( n_dim < k ) n_dim <- k
    Rho <- diag(n_dim)
    for ( i in 1:length(rho) ) {
      Rho[1,i+1] <- rho[i]
    }
    Rho[lower.tri(Rho)] <- Rho[upper.tri(Rho)]
    X.train <- mvrnorm( n=N , mu=rep(0,n_dim) , Sigma=Rho )
    X.test <- mvrnorm( n=N , mu=rep(0,n_dim) , Sigma=Rho )
    mm.train <- matrix(1,nrow=N,ncol=1)
    bnames <- "a"
    if ( k > 1 ) {
      mm.train <- cbind( mm.train , X.train[,2:k] )
      bnames <- c( "a" , paste("b",1:(k-1),sep="") )
      pnames <- paste("b",1:(k-1),sep="")
      P <- paste( "c(" , paste(pnames,collapse=",") , ")" , sep="" , collapse="" )
      Pf <- paste( P , "~ dnorm(0,",b_sigma,")" )
    }
    B <- paste( "c(" , paste(bnames,collapse=",") , ")" , sep="" , collapse="" )
    d <- list( y=X.train[,1] , mm=mm.train , Bvec=B )
    #m <- lm.fit( mm.train , X.train[,1] )
    flist <- list( y ~ dnorm( mu , 1 ) , mu ~ 0 + mm %*% eval(parse(text=Bvec)) )
    start.list <- list(a=0)
    if ( k>1 ) {
      flist[[3]] <- eval(parse(text=Pf))
      for ( i in 2:k ) {
        ptext <- paste( "b" , i-1 , sep="" , collapse="" )
        start.list[[i]] <- 0
        names(start.list)[i] <- ptext
      }
    }
    m <- map( flist , data=d , start=start.list )
    dev.train <- as.numeric( deviance(m) )
    mm.test <- matrix(1,nrow=N,ncol=1)
    if ( k > 1 ) mm.test <- cbind( mm.test , X.test[,2:k] )
    dev.test <- (-2)*sum( dnorm( X.test[,1] , mm.test %*% coef(m) , 1 , TRUE ) )
    # result
    result <- c( dev.train , dev.test )
    # DIC and WAIC
    if ( DIC==TRUE ) result <- c( result , as.numeric(DIC(m)) )
    if ( WAIC==TRUE ) {
      n_samples <- 1e3
      l <- link(m,n=n_samples,refresh=0)
      lppd <- 0
      pD <- 0
      for ( i in 1:N ) {
        ll <- dnorm( X.train[i,1] , l[,i] , 1 , TRUE )
        lppd <- lppd + log_sum_exp(ll) - log(n_samples)
        pD <- pD + var(ll)
      }
      result <- c( result , -2*(lppd-pD) )
    }
    if ( devbar==TRUE ) {
      post <- extract.samples( m , n=1e3 )
      dev <- sapply( 1:nrow(post) , function(i) 
        (-2)*sum( dnorm( X.train[,1] , mm.train %*% as.numeric(post[i,]) , 1 , TRUE ) )
      )
      result <- c( result , mean(dev) )
    }
    if ( devbarout==TRUE ) {
      post <- extract.samples( m , n=1e3 )
      dev <- sapply( 1:nrow(post) , function(i) 
        (-2)*sum( dnorm( X.test[,1] , mm.test %*% as.numeric(post[i,]) , 1 , TRUE ) )
      )
      result <- c( result , mean(dev) )
    }
    # return
    return(result)
  }
