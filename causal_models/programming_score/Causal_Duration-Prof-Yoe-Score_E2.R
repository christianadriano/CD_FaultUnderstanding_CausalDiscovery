"
Causal models that include the duration as covariate
"

library(rethinking)
library(stringr)
library(dplyr)

#Load data
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//load_consent_create_indexes_E2.R")
df_E2$profession_id <- as.integer(df_E2$profession_id)

# standardize variables = (zero centered, standard deviation one)
# df_E2$yoe <- scale(df_E2$years_programming, center=FALSE)
# df_E2$score <- scale(df_E2$qualification_score, center=FALSE)
df_E2$yoe <- df_E2$years_programming
df_E2$score <- df_E2$qualification_score

df_E2_aux <- df_E2


#-----------------
"OUTLIERS in DURATION"
"Code comprehension studies show that a programmer takes from 12 to 24 seconds is also the 
average minimum time to read one line of code."

"The lower cut to the minimum time to read all 5 questions and corresponding lines of code
in the qualification test. 
 Since the test has 5 questions, each question 
requires the inspection of one line of code, that would require the programmer from 60s to 120s.
We chose 60s (1 min) as the minimum time-effort one need to read and answer all 5 questions"

#The upper cut corresponds to 3 times the minimum. We choose 24s, so 60 min.

df_E2_aux <- df_E2[df_E2$testDuration_minutes<=12 & df_E2$testDuration_minutes>=1 ,]
boxplot(df_E2_aux$testDuration_minutes)
summary(df_E2_aux$testDuration_minutes)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   3.329   5.372   7.267   9.080  41.703

#scale without centering, so I avoid negative values
#df_E2_aux$testDuration_minutes <- scale(df_E2_aux$testDuration_minutes, center = FALSE) 
#---------------


#MODEL BUILDING

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
#       mean   sd  5.5%  94.5%
# a[1]  4.21 0.04  4.16  4.27
# a[2]  4.22 0.04  4.16  4.28
# a[3]  4.02 0.05  3.94  4.10
# a[4]  3.99 0.04  3.93  4.05
# a[5]  4.17 0.07  4.06  4.29
# bt    0.01 0.02 -0.02  0.03
# by    0.00 0.00  0.00  0.00
# sigma 0.81 0.01  0.80  0.83

#slopes are flat.

#Interactions
m3 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a[profession_id] +bt[profession_id]*testDuration_minutes,
    a[profession_id] ~ dnorm( 0 , 1.0 ),
    bt[profession_id] ~ dnorm( 0 , 1.0 ) ,
    sigma ~ dexp(1)
  ), data = df_E2_aux
) 

precis(m3,2)
"slope bt is flat for professionals, hobbyists, and others. 
It is slightly positive for undergrads (0.12),
which implies that 8.33 minutes would increase the score in 20%.
Conversely, it is slightly negative for graduate students (-0.26), 
which would imply that more time on the task would actually reduce their score,
more precisely 43.84 minutes would decresase the score in 20%.

These figures do not change in the model that we  include a slope for YoE (Mode m4)
"

#         mean   sd  5.5% 94.5%
# a[1]   4.21 0.03  4.17  4.26
# a[2]   4.22 0.03  4.17  4.27
# a[3]   4.06 0.05  3.98  4.14
# a[4]   3.98 0.04  3.92  4.04
# a[5]   4.18 0.06  4.07  4.28
# bt[1] -0.03 0.03 -0.08  0.02
# bt[2] -0.03 0.04 -0.09  0.03
# bt[3] -0.26 0.05 -0.34 -0.18
# bt[4]  0.12 0.03  0.08  0.16
# bt[5]  0.02 0.07 -0.09  0.14
# sigma  0.81 0.01  0.79  0.82

m4 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a[profession_id] + by[profession_id]*yoe +bt[profession_id]*testDuration_minutes,
    a[profession_id] ~ dnorm( 0 , 1.0 ),
    bt[profession_id] ~ dnorm( 0 , 1.0 ) ,
    by[profession_id] ~ dnorm( 0 , 1.0 ) ,
    sigma ~ dexp(1)
  ), data = df_E2_aux
) 

precis(m4,2)

#        mean   sd  5.5% 94.5%
# a[1]   4.27 0.04  4.20  4.34
# a[2]   4.29 0.05  4.22  4.37
# a[3]   3.79 0.08  3.66  3.91
# a[4]   3.81 0.05  3.73  3.90
# a[5]   3.89 0.11  3.72  4.06
# bt[1] -0.03 0.03 -0.08  0.02
# bt[2] -0.03 0.04 -0.09  0.03
# bt[3] -0.25 0.05 -0.33 -0.17
# bt[4]  0.14 0.03  0.10  0.18
# bt[5] -0.13 0.08 -0.26  0.00
# by[1] -0.01 0.00 -0.01  0.00
# by[2] -0.01 0.00 -0.01  0.00
# by[3]  0.06 0.01  0.04  0.09
# by[4]  0.04 0.01  0.02  0.05
# by[5]  0.02 0.01  0.01  0.03
# sigma  0.80 0.01  0.78  0.82

#VISUALIZE Coefficients with credible intervals
labels1 <- paste( "a[" , 1:5 , "]:" , levels(df_E2$profession) , sep="" )
labels3 <- paste( "bt[" , 1:5 , "]:" , levels(df_E2$profession) , sep="" )
labels4 <- paste( "by[" , 1:5 , "]:" , levels(df_E2$profession) , sep="" )

precis_plot( precis( m4 , depth=2 , pars=c("bt","by")) , 
             labels=c(labels3,labels4) ,
             xlab="qualification score" )
title("Model score = bt*duration + by*yoe")


m4.1 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a[profession_id] + ba[profession_id]*age+ by[profession_id]*yoe +bt[profession_id]*testDuration_minutes,
    a[profession_id] ~ dnorm( 0 , 1.0 ),
    ba[profession_id] ~ dnorm( 0 , 1.0 ),
    bt[profession_id] ~ dnorm( 0 , 1.0 ) ,
    by[profession_id] ~ dnorm( 0 , 1.0 ) ,
    sigma ~ dexp(1)
  ), data = df_E2_aux
) 

precis(m4.1,2)
#        mean   sd  5.5% 94.5%
# a[1]   5.25 0.14  5.03  5.47
# a[2]   4.17 0.14  3.94  4.40
# a[3]   3.90 0.27  3.47  4.33
# a[4]   4.21 0.14  3.98  4.45
# a[5]   3.05 0.20  2.73  3.37
# ba[1] -0.04 0.01 -0.05 -0.03
# ba[2]  0.00 0.00  0.00  0.01
# ba[3]  0.00 0.01 -0.02  0.02
# ba[4] -0.02 0.01 -0.03 -0.01
# ba[5]  0.03 0.01  0.02  0.04
# bt[1]  0.00 0.00  0.00  0.00
# bt[2]  0.00 0.00  0.00  0.00
# bt[3] -0.02 0.01 -0.03 -0.01
# bt[4]  0.00 0.00  0.00  0.00
# bt[5]  0.00 0.00  0.00  0.00
# by[1]  0.02 0.00  0.01  0.03
# by[2] -0.01 0.00 -0.02  0.00
# by[3]  0.07 0.02  0.04  0.09
# by[4]  0.03 0.01  0.01  0.04
# by[5] -0.01 0.01 -0.02  0.01
# sigma  0.78 0.01  0.76  0.80

compare(m4,m4.1,func=PSIS)
#               PSIS    SE dPSIS  dSE pPSIS weight
# m1.ages     6051.6 64.42     0   NA   3.7   0.46
# m1.ages.yoe 6052.7 64.87     1 2.15   4.6   0.27
# m1.yoe      6052.7 64.78     1 4.05   3.6   0.27
compare(m4,m4.1,func=WAIC)
#               WAIC    SE dWAIC  dSE pWAIC weight
# m1.ages     6051.6 64.49   0.0   NA   3.5   0.46
# m1.yoe      6052.5 64.77   0.9 4.05   3.4   0.29
# m1.ages.yoe 6052.8 64.85   1.2 1.95   4.7   0.25

#VISUALIZE Coefficients with credible intervals
labels1 <- paste( "a[" , 1:5 , "]:" , levels(df_E2$profession) , sep="" )
labels2 <- paste( "ba[" , 1:5 , "]:" , levels(df_E2$profession) , sep="" )
labels3 <- paste( "bt[" , 1:5 , "]:" , levels(df_E2$profession) , sep="" )
labels4 <- paste( "by[" , 1:5 , "]:" , levels(df_E2$profession) , sep="" )

precis_plot( precis( m4.1 , depth=2 , pars=c("ba","bt","by")) , 
             labels=c(labels2,labels3,labels4) ,
             xlab="qualification score" )
title("Model score = ba*age + bt*duration + by*yoe")


#Three way interaction
m5 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a[profession_id] + by[profession_id]*yoe +bt[profession_id]*testDuration_minutes + bty[profession_id]*testDuration_minutes*yoe,
    a[profession_id] ~ dnorm( 0 , 1.0 ),
    bt[profession_id] ~ dnorm( 0 , 1.0 ) ,
    by[profession_id] ~ dnorm( 0 , 1.0 ) ,
    bty[profession_id] ~ dnorm( 0 , 1.0 ) ,
    sigma ~ dexp(1)
  ), data = df_E2_aux
) 
precis(m5,2) #all slopes are flat.

#         mean   sd  5.5% 94.5%
# a[1]    4.23 0.03  4.19  4.27
# a[2]    4.23 0.03  4.18  4.29
# a[3]    4.04 0.05  3.96  4.12
# a[4]    4.01 0.04  3.96  4.07
# a[5]    4.19 0.07  4.09  4.30
# bty[1]  0.01 0.00  0.01  0.02
# bty[2]  0.02 0.00  0.01  0.03
# bty[3] -0.03 0.01 -0.05 -0.02
# bty[4]  0.04 0.01  0.03  0.06
# bty[5]  0.00 0.00 -0.01  0.00
# sigma   0.80 0.01  0.78  0.82
