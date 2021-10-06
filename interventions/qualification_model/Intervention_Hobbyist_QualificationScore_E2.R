"Hobbyist 
Intervention on test duration to falsify or estimate 
the effect on the adjusted qualification score

There are 4 causal graphs for Hobbyist that stem from two causal discovery methods (Score-based
and Constraint-based) and two groups of users slow and fast responders.

We probe each of these four causal graphs by formulating three types of interventions: 
test duration (TD), years of programming experience (YoE), and age (A).

Each formultion consists of a conditional expectation that we approximate by performing a 
multilevel regression using Markov Chain Monte Carlo API. This API is available in the 
probabilistic programming language STAN which we interface using the R Rethinking package.

"
install.packages(c("coda","mvtnorm","devtools","loo","dagitty"))
devtools::install_github("rmcelreath/rethinking")

remotes::install_github("rmcelreath/rethinking")

install.packages("Rtools")
install.packages("shape")

install.packages("ps")
install.packages("backports")
library(ps)
library(backports)
devtools::install_github("rmcelreath/rethinking")

remove.packages("rstan")
if (file.exists(".RData")) file.remove(".RData")

remotes::install_version("withr", "2.2.0")

#Sys.setenv(DOWNLOAD_STATIC_LIBV8 = 1) # only necessary for Linux without the nodejs library / headers
install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)


library(rethinking)
library(stringr)
library(dplyr)

f <- alist(
  y ~ dnorm( mu , sigma ),
  mu ~ dnorm( 0 , 10 ),
  sigma ~ dexp( 1 )
)

fit <- quap( 
  f , 
  data=list(y=c(-1,1)) , 
  start=list(mu=0,sigma=1)
)

summary(fit)
precis(fit)

fit_stan <- ulam( f , data=list(y=c(-1,1)) )
precis(fit_stan)

data(chimpanzees)
d <- list( 
  pulled_left = chimpanzees$pulled_left ,
  prosoc_left = chimpanzees$prosoc_left ,
  condition = as.integer( 2 - chimpanzees$condition ) ,
  actor = as.integer( chimpanzees$actor ) ,
  blockid = as.integer( chimpanzees$block )
)

m1 <- ulam(
  alist(
    pulled_left ~ bernoulli(theta),
    logit(theta) <- a + bp[condition]*prosoc_left  ,
    a ~ normal(0,4),
    bp[condition] ~ normal(0,1)
  ) ,
  data=d, chains=2, cores=1 , sample=TRUE )

precis(m1,depth=2)
plot(m1,depth=2)
pairs(m1)


#Load data
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E2.R")
df_E2 <- load_consent_create_indexes()
df_E2$profession_id <- as.integer(df_E2$profession_id)



#-----------------------------
#Test Duration

#Constraint-Based Graph


#Score-Based Graph

#Evaluation

#what proportion of causal effects (coefficients) were signficant?
#Accuracy of models
#Risk of overfitting


#-----------------------------
#Years of Programming

#-----------------------------
#Age

#-----------------------------
#Summary of comparisons

#Mean proportion of significant coefficients
#Mean Accuracy of models
#Mean Risk of overfitting




