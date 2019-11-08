"
Causal models that include Profession (Prf) and Years of Programming Experience (Yoe)
as covariates of the Programming Score obtained in the qualification test.

The goal of the current study is Is there any additional value in knowing a variable, once I already know all of
the other predictor variables?

So for example once you fit a multiple regression to predict programming score using both
years of experience and profession category, my model addresses the following questions:
(1) After I already know profession category, what additional predictive value do I gain 
by also knowing the number of years of experience in programming?
(2) After I already know the number of years of experience in programming, what additional 
value is there in also knowing the profession of the participant?
"

library(rethinking)
library(stringr)
library(dplyr)

#Load data
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//load_create_indexes_E2.R")

# standardize variables = (zero centered, standard deviation one)
df_E2$yoe <- scale(df_E2$years_programming)
df_E2$score <- scale(df_E2$qualification_score)

df_E2$profession_id <- as.integer(df_E2$profession_id)

#Model-1.1 only profession
m1.1 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a[profession_id],
    a[profession_id] ~ dnorm( 0 , 1 ) ,
    sigma ~ dexp(1)
  ), data = df_E2
) 


#Model-1.2 only Years of experience
m1.2 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- by*yoe,
    by ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp(1)
  ), data = df_E2
) 

#Model-2 both, but only additive effects
# Yoe->Score<-prof
m2 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a[profession_id]+by*yoe,
    a[profession_id] ~ dnorm( 0 , 1 ),
    by ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp(1)
  ), data = df_E2
) 


#Model-3 interaction term, prof influencing the effect of yoe on score
# yoe->score<-prof
# prof->score
m3.2 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a[profession_id] + bpy[profession_id]*yoe + by*yoe,
    by ~ dnorm( 0 , 0.5 ),
    a[profession_id] ~ dnorm( 0 , 1 ) ,
    by[profession_id] ~ dnorm( 0 , 0.5 ),
    bpy[profession_id] ~ dnorm( 0 , 0.5 ),
    sigma ~ dexp(1)
  ), data = df_E2
) 

m3.1 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a[profession_id] + bpy[profession_id]*yoe,
    a[profession_id] ~ dnorm( 0 , 1 ) ,
    bpy[profession_id] ~ dnorm( 0 , 0.5 ),
    sigma ~ dexp(1)
  ), data = df_E2
) 

m3.3 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a + by[profession_id]*yoe,
    a ~ dnorm( 0 , 1 ) ,
    by[profession_id] ~ dlnorm( 0 , 0.5 ),
    sigma ~ dexp(1)
  ), data = df_E2
) 

#------------------------------

labels1 <- paste( "a[" , 1:5 , "]:" , levels(df_E2$profession) , sep="" )
labels2 <- paste( "by[" , 1:5 , "]:" , levels(df_E2$profession) , sep="" )
labels3 <- paste( "bpy[" , 1:5 , "]:" , levels(df_E2$profession) , sep="" )


precis_plot( precis( m1.1 , depth=2 , pars=c("a")) , labels=labels1 ,
             xlab="qualification score" )
title("Model-1.1")

precis_plot( precis( m1.2 , depth=2 , pars=c("by"))  ,
             xlab="qualification score" )
title("Model-1.2")

precis_plot( precis( m2 , depth=2 , pars=c("a","by")) , labels=c(labels1,"by") ,
             xlab="qualification score" )
title("Model-2")

precis_plot( precis( m3.2 , depth=2 , pars=c("a","by","bpy")) , labels=c(labels1,labels2,labels3) ,
             xlab="qualification score" )
title("Model-3.2")

precis_plot( precis( m3.1 , depth=2 , pars=c("a","bpy")) , labels=c(labels1,labels3) ,
             xlab="qualification score" )
title("Model-3.1")

precis_plot( precis( m3.3 , depth=2 , pars=c("a","by")) , labels=c(labels1,"by") ,
             xlab="qualification score" )
title("Model-3.3")


#---------------------------------------------------------
"Plot the Posterior with corresponding variance (shaded region)"

#Generate simulated input data
Yoe_seq <- seq( from=min(df_E2$yoe) , to=max(df_E2$yoe) , length.out=50 )
Prof_seq <- seq( from=1, to=5)

#Plot the shade region with the variance
mu <- link(m1.1, data = data.frame(profession_id=Prof_seq))

sim1.1 <- sim(m1.1, data=list(profession_id=Prof_seq))

#Compute vectors of means
mu.mean = apply(mu,2,mean)
mu.PI = apply(sim1.1,2, PI, prob=0.89) #mean with the percentile intervals
mu.HPDI = apply(mu,2,HPDI, prob=0.89) #mean with highest posterior density interval

plot(score ~ profession_id, df_E2,col=col.alpha(rangi2,0.5)) #plot raw data
title(paste("M1.1 posterior score ","a[profession_id]"))

#plot the Map line and interval more visible
lines(Prof_seq,mu.mean)

#plot the shaded region with 89% HPDI
shade(mu.HPDI,Prof_seq)

#plot the shaded region with 89% PI
shade(mu.PI,Prof_seq)
#--------------
#Model 1.2
#Simulate the posterior with synthetic data
sim1.2 <- sim(m1.2, data=list(yoe=Yoe_seq))
mu <- link(m1.2, data = data.frame(yoe=Yoe_seq))

mu.mean = apply(mu,2,mean)
mu.PI = apply(sim1.2,2, PI, prob=0.89) #mean with the percentile intervals
mu.HPDI = apply(mu,2,HPDI, prob=0.89) #mean with highest posterior density interval

plot(score ~ yoe, df_E2,col=col.alpha(rangi2,0.5)) #plot raw data
title(paste("M1.2 posterior score ","score=by*yoe"))
lines(Yoe_seq,mu.mean)
shade(mu.HPDI,Yoe_seq)
shade(mu.PI,Yoe_seq)

"Results show that M1"

#--------------
#Model-2
#Plot the shade region with the variance
sim_2 <- sim(m2, data=list(profession_id=Prof_seq))
mu.PI = apply(sim_2,2, PI, prob=0.89) #compute the percentile intervals

plot(score ~ profession_id, df_E2,col=col.alpha(rangi2,0.5)) #plot raw data
title(paste("Model-2, Posterior: ",levels(df_E2$profession)[1]))

#plot the Map line and interval more visible
lines(Prof_seq,mu.mean)

#plot the shaded region with 89% HPDI
shade(mu.HPDI,Prof_seq)

#plot the shaded region with 89% PI
shade(mu.PI,Prof_seq)

