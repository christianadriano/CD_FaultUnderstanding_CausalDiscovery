"
Causal models that include the duration as covariate
"

library(rethinking)
library(stringr)
library(dplyr)

#Load data
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//load_create_indexes_E2.R")

# standardize variables = (zero centered, standard deviation one)
#df_E2$yoe <- scale(df_E2$years_programming)
#df_E2$score <- scale(df_E2$qualification_score)

df_E2$score <-  df_E2$qualification_score
df_E2$yoe <- df_E2$years_programming
summary(df_E2$testDuration)
boxplot(df_E2$testDuration)

df_E2$testDuration_minutes <- df_E2$testDuration/(60 *1000)
boxplot(df_E2$testDuration_minutes)
maximum <- 60
max <- 1625504839/1

#removing outliers (tests above 60 minutes)
df_E2_aux <- df_E2[df_E2$testDuration_minutes<=60,]
boxplot(df_E2_aux$testDuration_minutes)

sd(df_E2_aux$testDuration_minutes)
#>[1] 7.731263

#Model Design

"PRIORS - Regarding the priors, I believe the relationship of the covariates with
the qualification score is weak. Hence, I am using a prior that considers that 95%
of the cases (two standard deviations) will be below 2.
"


#Model-2 both, but only additive effects
# Yoe->Score<-prof
m2 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a[profession_id]+by*yoe + bt*testDuration_minutes,
    a[profession_id] ~ dnorm( 0 , 1.0 ),
    bt ~ dnorm( 0 , 1.0 ) ,
    by ~ dnorm( 0 , 1.0 ) ,
    sigma ~ dexp(1)
  ), data = df_E2_aux
) 
precis(m2,2)
# mean   sd 5.5% 94.5%
# a[1]  4.20 0.04 4.14  4.26
# a[2]  4.19 0.04 4.12  4.26
# a[3]  3.99 0.05 3.90  4.07
# a[4]  3.93 0.04 3.87  4.00
# a[5]  4.13 0.07 4.01  4.24
# bt    0.00 0.00 0.00  0.01
# by    0.00 0.00 0.00  0.00
# sigma 0.81 0.01 0.79  0.83

#Interactions
m3 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a[profession_id] + by[profession_id]*yoe + bt[profession_id]*testDuration_minutes,
    a[profession_id] ~ dnorm( 0 , 1.0 ),
    bt ~ dnorm( 0 , 1.0 ) ,
    by ~ dnorm( 0 , 1.0 ) ,
    sigma ~ dexp(1)
  ), data = df_E2_aux
) 
