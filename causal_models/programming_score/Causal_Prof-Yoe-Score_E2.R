"
Causal models that include Profession (Prf) and Years of Programming Experience (Yoe)
as covariates of the Programming Score obtained in the qualification test.

The goal of the current study is to know 
If there is any additional value in knowing a variable, once I already know all of the other predictor variables?

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
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E2.R")

df_consent <- load_consent_create_indexes()
#Remove NA's
df_E2 <- df_consent[complete.cases(df_consent[,c("years_programming","qualification_score")]),]



# standardize variables = (zero centered, standard deviation one)
df_E2$yoe <- scale(df_E2$years_programming)
df_E2$score <- scale(df_E2$qualification_score)
#df_E2$yoe <- df_E2$years_programming
#df_E2$score <-  df_E2$qualification_score


#df_E2$profession_id <- as.integer(df_E2$profession_id)

#Model-1.1 only profession
m1.1 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a[profession_id],
    a[profession_id] ~ dnorm( 0 , 1 ) ,
    sigma ~ dexp(1)
  ), data = df_E2
) 
precis(m1.1, depth = 2)

#Model-1.2 only Years of experience
m1.2 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- by*yoe,
    by ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp(1)
  ), data = df_E2
) 
precis(m1.2)

compare(m1.2,m1.1)

#Model-2 both, but only additive effects
# Yoe->Score<-prof
m2 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a[profession_id]+by*yoe,
    a[profession_id] ~ dnorm( 0 , 0.5 ),
    by ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp(1)
  ), data = df_E2
) 
precis(m2,depth=2)

#INTERACTION MODELS
#Model-3 interaction term, prof influencing the effect of yoe on score
# yoe->score<-prof
# prof->score

m3.1 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a[profession_id] + bpy[profession_id]*yoe,
    a[profession_id] ~ dnorm( 0 , 1.0 ) ,
    bpy[profession_id] ~ dnorm( 0 , 1.0 ),
    sigma ~ dexp(1)
  ), data = df_E2
) 
precis(m3.1,depth=2)



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
precis(m3.1,depth=2)


m3.3 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a + by[profession_id]*yoe,
    a ~ dnorm( 0 , 1 ) ,
    by[profession_id] ~ dlnorm( 0 , 0.5 ),
    sigma ~ dexp(1)
  ), data = df_E2
) 
precis(m3.3,depth=2)

#--------------------------------------
#COMPARING MODELS

c <- compare(m1.1, m1.2, m2, m3.1, m3.2, m3.3, sort="WAIC", func=WAIC)
c
#       WAIC    SE dWAIC   dSE pWAIC weight
# m3.1 4846.0 45.80   0.0    NA  11.9   0.91 <-----------BETTER
# m3.2 4851.5 45.74   5.5  4.42  16.5   0.06
# m2   4852.9 45.27   6.9  8.27   7.1   0.03
# m1.2 4878.2 43.61  32.2 15.09   1.5   0.00
# m3.3 4881.9 44.29  35.9 14.32   6.4   0.00
# m1.1 4984.1 42.27 138.0 22.50   6.6   0.00

plot(c)
#Plot shows that,except for m1.1, all other models have an overlap of WAIC within +/-SE
#So might still choose a simpler model m2, which is without interactions.

"Compare m3.3 with m3.1, because in the former the credible interval for all by do 
not cross zero, but the intercept a crosses. Meanwhile, in m3.1, all but graduates 
do not cross zero, whereas the three intercepts do not cross zero (professionals,
graduates, and others)"
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

#sample from the posterior distribution, and then compute
#for each case in the data and sample from the posterior distribution.
mu <- link(m1.1, data = data.frame(profession_id=Prof_seq))
#Compute vectors of means
mu.mean = apply(mu,2,mean)
mu.HPDI = apply(mu,2,HPDI, prob=0.89) #mean with highest posterior density interval

#Simulates score by extracting from the posterior, but now also
#considers the variance
sim1.1 <- sim(m1.1, data=list(profession_id=Prof_seq)) 
score.PI = apply(sim1.1,2, PI, prob=0.89) #mean with the percentile intervals

plot(score ~ profession_id, df_E2,col=col.alpha(rangi2,0.5)) #plot raw data
title(paste("M1.1 posterior score ","a[profession_id]"))

#plot the Map line and interval more visible
lines(Prof_seq,mu.mean)

#plot the shaded region with 89% HPDI
shade(mu.HPDI,Prof_seq)

#plot the shaded region with 89% PI
shade(score.PI,Prof_seq)
#--------------
#Model 1.2
#Simulate the posterior with synthetic data
sim1.2 <- sim(m1.2, data=data.frame(yoe=Yoe_seq))
mu <- link(m1.2, data = data.frame(yoe=Yoe_seq))

mu.mean = apply(mu,2,mean)
mu.PI = apply(sim1.2,2, PI, prob=0.89) #mean with the percentile intervals
mu.HPDI = apply(mu,2,HPDI, prob=0.89) #mean with highest posterior density interval

plot(score ~ yoe, df_E2,col=col.alpha(rangi2,0.5)) #plot raw data
title(paste("M1.2 posterior score ","score=by*yoe"))
lines(Yoe_seq,mu.mean, col="red")
shade(mu.HPDI,Yoe_seq)
shade(mu.PI,Yoe_seq)

"Results show that M1"

#--------------
#Model-2
#Plot the shade region with the variance

#Simulates score by extracting from the posterior, but now also
#considers the variance
sim_scores <- sim(m2, data=data.frame(profession_id=3,yoe=Yoe_seq))
score.PI = apply(sim_scores,2, PI, prob=0.89) #mean with the percentile intervals

mu <- link(m2, data = data.frame(profession_id=3,yoe=Yoe_seq))
mu.mean2 = apply(mu,2,mean)
mu.HPDI = apply(mu,2,HPDI, prob=0.89) #mean with highest posterior density interval

plot(score ~ yoe, df_E2,col=col.alpha(rangi2,0.5)) #plot raw data
title(paste("Model-2, a[profession_id]+by*yoe ",""))

#plot the Map line and interval more visible
lines(Yoe_seq,mu.mean2,col="blue")

#plot the shaded region with 89% HPDI
shade(mu.HPDI,Yoe_seq)

#plot the shaded region with 89% PI
shade(mu.PI,Yoe_seq)

#-----------------
#Model 3.1

sim3.1 <- sim(m3.1, data=data.frame(profession_id=1,yoe=Yoe_seq))
mu.PI = apply(sim3.1,2, PI, prob=0.89) #mean with the percentile intervals

mu <- link(m1.2, data = data.frame(profession_id=4,yoe=Yoe_seq))
mu.mean_4 <-  apply(mu,2,mean)
mu.HPDI = apply(mu,2,HPDI, prob=0.89) #mean with highest posterior density interval

plot(score ~ yoe, df_E2,col=col.alpha(rangi2,0.5)) #plot raw data
title(paste("M3.1","score=a[i] + by[i]*yoe"))
lines(Yoe_seq,mu.mean, col="blue")
shade(mu.HPDI,Yoe_seq)

shade(mu.PI,Yoe_seq)



#-----------------
# Plotting the variance around the mean for
#professsional and years of experience =3
post3.1 <- extract.samples(m3.1)
mu_at_yoe_3 <- post3.1$a[,1] + post3.1$bpy[,1]*3
dens( mu_at_yoe_3 , col=rangi2 , lwd=2 , xlab="mu|yoe=3" )
HPDI(mu_at_yoe_3,prob = 0.89)
  #   |0.89    0.89| 
  # 3.991492 4.244820 

#Two implications of the model m3.1 for professionals:
# - more experience decreases the score because the
#slope in the regression model is negative
# - the slope is almost horizontal
dens( post3.1$bpy[,1] , col=rangi2 , lwd=2 , xlab="bpy professionals" )

