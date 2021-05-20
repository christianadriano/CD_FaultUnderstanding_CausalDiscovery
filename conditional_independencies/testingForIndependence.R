"
Testing for independence

The code here runs the d-variable Hilbert Schmidt independence criterion (dHSIC).
This method measures the dependency between variables. It is a non-parametric method,
hence it does not assume normality distribution.

large sample limit the value of dHSIC is 0 if the variables are jointly independent and 
positive if there is a dependence. See https://rdrr.io/cran/dHSIC/man/dhsic.html

"

install.packages("dHSIC") 
install.packages("mgcv")

library(dHSIC)
library(mgcv)

set.seed(1)
X <- rnorm(200)
Y <- X^3 + rnorm(200)

modelforw <- gam(Y ~s(X)) #Y as function of X, Y=f(X)

modelback <- gam(Y ~s(Y)) #Y as function of X, X=f(Y)

#independence test
dhsic.test(modelforw$residuals,X)$p.value
#[1] 0.7932068, not statistically significant, hence failed to 
#disprove the null hypothesis that the residuals and X have distinct averages
#This means that Y and X in modelforw are not independent

dhsic.test(modelback$residuals,Y)$p.value
#[1] 0.000999001, statistically significant, hence Y and X in modelback ARE INDEPENDENT


"
Comparing multiple causal relations. 
We can use maximum likelihood to compute  

"

#Computing likelihood of models
- log(var(X)) - log(var(modelforw$residuals))
#[1] 0.1420063, which is much better than the one below

 - log(var(modelback$residuals)) - log(var(Y))
#[1] 66.23279