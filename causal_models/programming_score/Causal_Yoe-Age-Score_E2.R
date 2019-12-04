"
Causal Inference Experiment-2
Outcome: Programming Score (score)
Covariates: Years of Programming (yoe), Age

Implication of a causal model
m1.1 yoe <- a + ba*ages (correlation coeficient)

Models: 
m1.2 <- a + by*yoe (total effect of Years of experience)
m1.3 <-  a+ ba*ages (total effect of age)

m1.4 <-  a+ ba*ages +by*yoe
m1.5.1 <-  a+ ba*ages +by*yoe + ba*by*yoe

generalization by gender and country
m1.6 <- a + by[gender_id]*yoe + ba[gender_id]*ages
m1.7 <- a + by[country_id]*yoe + ba[country_id]*ages

"

library(rethinking)
library(stringr)
library(dplyr)
library(ggdag)
library( dagitty )
library (ggm)
library(loo) #for running WAIC and Pareto-Smooth Leave One Out Cross-Validation
library(mvtnorm)
library(devtools)

#Load data
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//load_consent_create_indexes_E2.R")

"Remove participants for whom we do not have years of experience information  (who did not complete the survey)"
df <- df_E2[complete.cases(df_E2[,"years_programming"]),] #initial 3657, left with 2062 rows

"Outlier in Age. Removing participants who reported to be below 18 years old."
df <- df[df$age>=18,] #removed one, left with 2061 rows

"Outlier in Yoe. Removing participants which the difference between age and yoe is less than ten
years-old"
age_minus_yoe <- df$age-df$years_programming
minimum_age_minus_yoe <- age_minus_yoe>=12
df <- df[minimum_age_minus_yoe,] #left with 2040 rows

#----------------------
#Rename fields to be easier to place in formulas
df$yoe <- scale(df$years_programming)
df$ages <- scale(df$age)

boxplot(df$yoe)

#Causal graph
#Create
dag1.1 <- dagitty( "dag {
score [outcome];
age [exogenous];
yoe [endogenous];
age -> score;
yoe -> score;
age -> yoe 
}")

coordinates(dag1.1) <- list( x=c(yoe=0,score=1,age=2) , y=c(yoe=0,score=1,age=0) )
plot( dag1.1 )
tidy_dagitty(dag1.1)
ggdag(dag1.1, layout = "circle")

condIndep <- impliedConditionalIndependencies(dag1.1)
#{}

#Conditional independence assumptions
paths(dag1.1,c("age"),"score",directed = TRUE)
# $paths [1] "age -> score"        "age -> yoe -> score"
# $open [1] TRUE TRUE

adjustmentSets(dag1.1,exposure = "age",outcome = "score",effect = c("direct"))
#{ yoe } because YoE is a mediator
adjustmentSets(dag1.1,exposure = "yoe",outcome = "score",effect = c("direct"))
#{ age } because age is a confounder



#Are Age and YoE correlated? YES!
m_correl <- quap(
  alist(
    yoe ~ dnorm( mu , sigma ) ,
    mu <- a + ba*ages,
    ba ~ dnorm( 0 , 1 ) ,
    a ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = df
) 
precis(m_correl)
  # mean   sd  5.5% 94.5%
  # ba    0.27 0.02  0.24  0.30
  # a     0.00 0.02 -0.03  0.03
  # sigma 0.96 0.02  0.94  0.99

"The slope that explain yoe by ages has non-zero value in the credible interval. 
The mean of the slope correspond to a weak correlation strenght 
(scale from 0.1 to 0.3). This slope ba=0.27 tells that for each year of age there is 
an average gain of 3 months of Yoe (0.27 year). Now we looked at the possibility of
age being a confounder. "


#-------------------------------------------------------------

"Remove people who did no qualify to the test (score<2)"
df <- df[complete.cases(df[,"qualification_score"]),]
"Scale score and rename"
df$score <- df$qualification_score #scale(df$qualification_score)
table(df$score)
#Score         0   1   2   3   4   5 
#Participants 240 402 280 157 158 183 
df_qualified <- df[df$score>=3,]
table(df_qualified$score)

m1.0 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a + by*yoe,
    by ~ dnorm( 0 , 1 ) ,
    a ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = df_qualified
) 
precis(m1.0)
#       mean   sd 5.5% 94.5%
# by    0.06 0.03 0.02  0.11
# a     4.02 0.04 3.95  4.08
# sigma 0.82 0.03 0.78  0.86

#All who took the test
m1.1 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a + by*yoe,
    by ~ dnorm( 0 , 1 ) ,
    a ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = df
) 
precis(m1.1)

#Considering all who took the test
#       mean   sd 5.5% 94.5%
# by    0.46 0.04 0.40  0.52
# a     2.05 0.04 2.00  2.14
# sigma 1.55 0.03 1.50  1.60
"Model m1.0 tells that for each year of programming experience there is an increase in
0.06 in score, whereas in m1.1 there is a gain of almost half a score point (0.46).
Assuming nothing changes, except yoe, someone who got zero score, would need 6 yoe to qualify (3/0.46)"

#--------
"Since the people have different ages, which means that their Yoe might have been gained
at different moments in their lives, we looked if age has an effect on score."


#Total effect of Age on Score
m1.2 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a+ ba*ages,
    ba ~ dnorm( 0 , 0.5 ) ,
    a ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = df
) 
precis(m1.2)
#       mean   sd  5.5% 94.5%
# ba    -0.06 0.03 -0.11  0.00
# a      2.45 0.03  2.40  2.50
# sigma  0.72 0.02  0.68  0.76

"Age has a negative effect, but it is uncertain as it crosses zero in in the 89%
credible interval. Since age could be confounder of the effect of Yoe on score,
we looked at the correlation between Age and Yoe."


rethinking::compare(m1.1,m1.2, func=PSIS) 
#PSIS = Pareto-smoothed importance sampling
#        PSIS    SE dPSIS   dSE pPSIS weight
# m1.1 5284.1 42.52   0.0    NA   2.6      1
# m1.2 5425.2 38.64 141.1 20.77   2.5      0

rethinking::compare(m1.1,m1.2, func=WAIC)
#WAIC = Widely Applicable Information Criteria
#        WAIC    SE dWAIC   dSE pWAIC weight
# m1.1 5284.0 42.59     0    NA   2.6      1
# m1.2 5425.1 38.61   141 20.79   2.5      0

#Conditioning both on Age and YoE
m1.4 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a + ba*ages + by*yoe,
    by ~ dnorm( 0 , 1 ) ,
    ba ~ dnorm( 0 , 1 ) ,
    a ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = df
) 
precis(m1.4)
#        mean   sd  5.5% 94.5%
# by     0.20 0.03  0.15  0.26
# ba    -0.13 0.03 -0.19 -0.08
# a      2.44 0.03  2.40  2.50
# sigma  0.69 0.02  0.66  0.73
"After deconfounding, we can see that the effects of yoe and age got clearer.
The effect of yoe got stronger (by in m1.1 versus m1.4). The effect 
of age got stronger and its credible interval outside zero (m1.1 versus m1.4)"

rethinking::compare(m1.1,m1.2,m1.4, func=PSIS) 
#       PSIS    SE dPSIS   dSE pPSIS weight
# m1.4 5330.4 42.72   0.0    NA   3.5      1 
# m1.1 5355.4 42.52  25.0  9.42   2.4      0
# m1.2 5505.1 38.44 174.8 24.23   2.5      0
rethinking::compare(m1.1,m1.2,m1.4, func=WAIC) 
#        WAIC    SE dWAIC   dSE pWAIC weight
# m1.4 5330.2 42.70   0.0    NA   3.4      1
# m1.1 5355.6 42.35  25.4  9.35   2.5      0
# m1.2 5505.2 38.49 175.0 24.27   2.5      0


#------------------------------------
"Analyse of Interactions. Does age also has an influence on the strenght of the 
effect of yoe on score? We evaluate interactions for analyze this hypothesis."
m1.5.1 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a + bya*yoe*ages +ba*ages +by*yoe,
    bya ~ dnorm( 0 , 0.5 ) ,
    ba ~ dnorm( 0 , 0.5 ) ,
    by ~ dnorm( 0 , 0.5 ) ,
    a ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = df
) 
precis(m1.5.1)

m1.5.2 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a + bya*yoe*ages +ba*ages,
    bya ~ dnorm( 0 , 0.5 ) ,
    ba ~ dnorm( 0 , 0.5 ) ,
    a ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = df
) 

precis(m1.5.2)


m1.5.3 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a + bya*yoe*ages,
    bya ~ dnorm( 0 , 0.5 ) ,
    a ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = df
) 

precis(m1.5.3)

m1.5.4 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a + bya*yoe*ages + by*yoe,
    bya ~ dnorm( 0 , 0.5 ) ,
    by ~ dnorm( 0 , 0.5 ) ,
    a ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = df
)


"About interaction YoE and Age
Interaction is positive only when a term for yoe is not present (m.1.5.2, bya=0.05 [0.02,0.08] and
m1.5.3 bya==0.03 [0.02,0.08]. 
However, when the slope for yoe (by) is present, the interaction term can present zero value 
in their credible intervals (m1.5.1, bya=-0.03 [], m1.5.4, bya=-0.04 [-0.07,0.00]). 
Meanwhile in these models m1.5.1 and m1.5.4, by has positive values for all the
credible intervals.

These models do not show that participants' age influence the effect of yoe on score, i.e.,
we could not see any moderation effect of age.
"

"OVERFITTING Evaluation
Models with age, yoe,and interaction between yoe and age performed better both in terms 
of parameters being  within credible intervals as well as presenting lower overfitting 
measures (PSIC and WAIC, which agreed with other)."

rethinking::compare(m1.5.1, m1.5.2, m1.5.3, m1.5.4, m1.4, func=PSIS)
#          PSIS    SE dPSIS   dSE pPSIS weight
# m1.5.1 5321.0 43.37   0.0    NA   4.2   0.99
# m1.4   5330.2 42.78   9.3  5.13   3.4   0.01
# m1.5.4 5343.4 43.00  22.5  8.85   3.1   0.00
# m1.5.3 5470.2 39.65 149.3 23.31   2.6   0.00
# m1.5.2 5470.9 39.57 150.0 23.29   3.4   0.00
rethinking::compare(m1.5.1, m1.5.2, m1.5.3, m1.5.4, m1.4, func=WAIC)
#          WAIC    SE dWAIC   dSE pWAIC weight
# m1.5.1 5320.8 43.40   0.0    NA   4.1   0.99
# m1.4   5330.3 42.81   9.6  5.21   3.4   0.01
# m1.5.4 5343.6 43.02  22.8  8.77   3.2   0.00
# m1.5.3 5470.0 39.55 149.2 23.40   2.5   0.00
# m1.5.2 5470.7 39.61 150.0 23.41   3.3   0.00

#Plotting models m1.4 and m1.5.1



#-------------------------------------
"Generalization. Do these models generalize to subgroups of participants, or 
in other words, are there other known variables that could also be confounders
of the effect of yoe and age on score? Next we look at gender and country"


#GENDER
#Conditioning both on Age and YoE and on Gender (indicador variable)
m1.6 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a[gender_id] + ba[gender_id]*ages + by[gender_id]*yoe,
    by[gender_id] ~ dnorm( 0 , 1 ) ,
    ba[gender_id] ~ dnorm( 0 , 1 ) ,
    a[gender_id] ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = df
) 
precis(m1.6,depth=2)

labels1 <- paste( "a[" , 1:3 , "]:" , levels(df$gender) , sep="" )
labels2 <- paste( "by[" , 1:3  , "]:" , levels(df$gender) , sep="" )
labels3 <- paste( "ba[" , 1:3  , "]:" , levels(df$gender) , sep="" )

precis_plot( precis( m1.6 , depth=2 , pars=c("by","ba","a")) , 
             labels=c(labels2,labels3,labels1),xlab="qualification score" )
title("Model1.6 conditioned on age, yoe, and gender")

"The previous results generalize for male and female gender groups, as
the coeficients for yoe and age are respectively non-negative and 
non-positive (Table-x). This is not true for the prefer_not_tell group, 
maybe because it contained only 5 participants."

#-----------
#COUNTRY
#Conditioning both on Age and YoE and on Country (indicador variable)
m1.7 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a[country_id] + ba[country_id]*ages + by[country_id]*yoe,
    by[country_id] ~ dnorm( 0 , 1 ) ,
    ba[country_id] ~ dnorm( 0 , 1 ) ,
    a[country_id] ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = df
) 
precis(m1.7,depth=2)

labels1 <- paste( "a[" , 1:3 , "]:" , levels(df$country_labels) , sep="" )
labels2 <- paste( "by[" , 1:3  , "]:" , levels(df$country_labels) , sep="" )
labels3 <- paste( "ba[" , 1:3  , "]:" , levels(df$country_labels) , sep="" )

precis_plot( precis( m1.7 , depth=2 , pars=c("ba","by","a")) , 
             labels=c(labels2,labels3,labels1),xlab="qualification score" )
title("Model1.7 conditioned on age, yoe, and country")

"The results generalize for US and Other country groups, but not for INDIA."
