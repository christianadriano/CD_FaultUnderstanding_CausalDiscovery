" use o invariant causal prediction "


install.packages("InvariantCausalPrediction")
library(InvariantCausalPrediction)

##########################################
####### 1st example: SOURCE: https://cran.r-project.org/web/packages/InvariantCausalPrediction/InvariantCausalPrediction.pdf
####### Simulate data with interventions
set.seed(1)
## sample size n
n <- 2000
## 4 predictor variables
p <- 4
## simulate as independent Gaussian variables
X <- matrix(rnorm(n*p),nrow=n)
## divide data into observational (ExpInd=1) and interventional (ExpInd=2)
ExpInd <- c(rep(1,n/2),rep(2,n/2))
## for interventional data (ExpInd==2): change distribution
nI <- sum(ExpInd==2)
X[ExpInd==2,] <- X[ExpInd==2,] + matrix( 5*rt( nI*p,df=3),ncol=p)
## add hidden variables
W <- rnorm(n) * 5
X <- X + outer(W, rep(1,4))
## first two variables are the causal predictors of Y
beta <- c(1,1,0,0)
## response variable Y
Y <- as.numeric(X%*%beta - 2*W + rnorm(n))

####### Compute "hidden Invariant Causal Prediction" Confidence Intervals
icp <- hiddenICP(X,Y,ExpInd,alpha=0.01)
print(icp)
###### Print point estimates and points in the confidence interval closest to 0
print(icp$betahat)
print(icp$maximinCoefficients)
cat("true coefficients are:", beta)
#### compare with coefficients from a linear model
cat("coefficients from linear model:")
print(summary(lm(Y ~ X-1)))

      