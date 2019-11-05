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

#Model-0 Intercept only
m0 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a[profession_id],
    a[profession_id] ~ dnorm( 0 , 1 ) ,
    sigma ~ dexp(1)
  ), data = df_E2
) 


#Model-1 No interaction, only additive effects
m1 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a[profession_id] + by[profession_id]*yoe,
    a[profession_id] ~ dnorm( 0 , 1 ) ,
    by[profession_id] ~ dnorm( 0 , 0.5 ),
    sigma ~ dexp(1)
  ), data = df_E2
) 


#Model-2 prior for by positive
m2 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a[profession_id] + by[profession_id]*yoe,
    a[profession_id] ~ dnorm( 0 , 1 ) ,
    by[profession_id] ~ dlnorm( 0 , 0.5 ),
    sigma ~ dexp(1)
  ), data = df_E2
) 


#------------------------------

labels1 <- paste( "a[" , 1:5 , "]:" , levels(df_E2$profession) , sep="" )
labels2 <- paste( "b[" , 1:5 , "]:" , levels(df_E2$profession) , sep="" )
labels <- c(labels1,labels2)


precis_plot( precis( m0 , depth=2 , pars=c("a")) , labels=labels ,
             xlab="qualification score" )
title("Model-0")

precis_plot( precis( m1 , depth=2 , pars=c("a","by")) , labels=labels ,
             xlab="qualification score" )
title("Model-1")

precis_plot( precis( m2 , depth=2 , pars=c("a","by")) , labels=labels ,
             xlab="qualification score" )
title("Model-2")


precis(m1,depth=2)



#---------------------------------------------------------
#Dead code

# df_E2$prf <- case_when(
#   df_E2$profession=="Professional_Developer" ~ 5,
#   df_E2$profession=="Hobbyist" ~ 4,
#   df_E2$profession=="Graduate_Student" ~ 3,
#   df_E2$profession=="Undergraduate_Student" ~ 2,
#   df_E2$profession=="Other" ~ 1
# )
