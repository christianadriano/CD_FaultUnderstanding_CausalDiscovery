" 

Bayesian Linear Regression experiments with the package Rethinking from McElreath

https://github.com/rmcelreath/rethinking

"

install.packages(c("mvtnorm","loo","coda"), repos="https://cloud.r-project.org/",dependencies=TRUE)
options(repos=c(getOption('repos'), rethinking='http://xcelab.net/R'))
install.packages('rethinking',type='source')

remove.packages("rstan")
if (file.exists(".RData")) file.remove(".RData")
