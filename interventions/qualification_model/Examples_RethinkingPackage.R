"
Rethinking Package Examples 
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
