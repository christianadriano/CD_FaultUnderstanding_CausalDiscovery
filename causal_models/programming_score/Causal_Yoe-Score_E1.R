"
Causal Inference Programming Score Experiment-1
"

library(rethinking)
library(stringr)
library(dplyr)

#Load data
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//load_consent_create_indexes_E1.R")

"Remove participants for whom we do not have years of experience information  (who did not complete the survey)"
df <- df_E1[complete.cases(df_E1[,"years_programming"]),] #left with 486 rows

"Remove people who did no qualify to the test (score<2)"
df <- df[df$qualification_score>=2,] #left with 483 rows

"Outlier in Age. Removing participants who reported to be below 18 years old."
df <- df[df$age>=18,] #removed one, left with 482 rows

"Outlier in Yoe. Removing participants which the difference between age and yoe is less than ten
years-old"
age_minus_yoe <- df$age-df$years_programming
minimum_age_minus_yoe <- age_minus_yoe>=10
df <- df[minimum_age_minus_yoe,] #left with 479 rows

#----------------------
#Rename fields to be easier to place in formulas
df$yoe <- scale(df$years_programming)
df$score <- df$qualification_score #scale(df$qualification_score)
df$ages <- scale(df$age)

boxplot(df$yoe)
table(df$score)
#score:     2    3   4 
#subjects: 328  85  65 

#Direct effect of Years of experience
m1.1 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a + by*yoe,
    by ~ dnorm( 0 , 0.5 ) ,
    a ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = df
) 
precis(m1.1)
# mean   sd 5.5% 94.5%
# by    0.15 0.03 0.10  0.20
# a     2.44 0.03 2.39  2.49
# sigma 0.70 0.02 0.67  0.74
"Model m1.1 tells that for each year of programming experience there is an increase in
0.15 in score. Assuming nothing changes, but yoe, someone who got zero score, would need 13.3 yoe to qualify (2/0.15)
Since the people have different ages, which means that their Yoe might have been gained
at different moments in their lives, we looked if age has an effect on score."

#Direct effect of Age on Score
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
# a      2.44 0.03  2.39  2.49
# sigma  0.72 0.02  0.68  0.76

"Age has a negative effect, but it is uncertain as it crosses zero in in the 89%
credible interval. Since age could be confounder of the effect of Yoe on score,
we looked at the correlation between Age and Yoe."

#Are Age and YoE correlated? YES, ba
m1.3 <- quap(
  alist(
    yoe ~ dnorm( mu , sigma ) ,
    mu <- a + ba*ages,
    ba ~ dnorm( 0 , 0.5 ) ,
    a ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = df
) 
precis(m1.3)
#       mean   sd  5.5% 94.5%
# ba    0.36 0.04  0.30  0.43
# a     0.00 0.04 -0.07  0.07
# sigma 0.93 0.03  0.88  0.98
"The slope that explain yoe by ages has non-zero value in the credible interval. 
The mean of the slope correspond to a medium correlation strenght in Cohen scale
(from 0.3 to 0.5). The slope tells that for each year of age there is an average 
gain of 4 months of Yoe (0.36 year). Now we looked at the possibility of
age being a confounder. "

#Conditioning both on Age and YoE
m1.4 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a + ba*ages + by*yoe,
    by ~ dnorm( 0 , 0.5 ) ,
    ba ~ dnorm( 0 , 0.5 ) ,
    a ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = df
) 
precis(m1.4)
#        mean   sd  5.5% 94.5%
# by     0.20 0.03  0.14  0.25
# ba    -0.13 0.03 -0.18 -0.07
# a      2.44 0.03  2.39  2.49
# sigma  0.69 0.02  0.66  0.73
"After deconfounding, we can see that it got more clear the effects of yoe and age.
The effect of yoe got stronger (by in m1.1 versus m1.4). The effect 
of age got stronger and its credible interval outside zero (m1.1 versus m1.4)"


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

m1.5.3 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a + bya*yoe*ages,
    bya ~ dnorm( 0 , 0.5 ) ,
    a ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = df
) 

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

precis(m1.5.4)

"About interaction YoE and Age
Interaction is positive only when a term for yoe is not present (m.1.5.2, bya=0.05 [0.02,0.08] and
m1.5.3 bya==0.03 [0.02,0.08]. 
However, when the slope for yoe (by) is present, the interaction term can present zero value 
in their credible intervals (m1.5.1, bya=-0.03 [], m1.5.4, bya=-0.04 [-0.07,0.00]). 
Meanwhile in these models m1.5.1 and m1.5.4, by has positive values for all the
credible intervals.

We interpret from these models that the effect on yoe on score is not influence by the
age of the participant.
"

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
    by[gender_id] ~ dnorm( 0 , 0.5 ) ,
    ba[gender_id] ~ dnorm( 0 , 0.5 ) ,
    a[gender_id] ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = df
) 
precis(m1.6,depth=2)

#labels1 <- paste( "a[" , 1:3 , "]:" , levels(df$gender) , sep="" )
labels2 <- paste( "by[" , 1:3  , "]:" , levels(df$gender) , sep="" )
labels3 <- paste( "ba[" , 1:3  , "]:" , levels(df$gender) , sep="" )

precis_plot( precis( m1.6 , depth=2 , pars=c("by","ba")) , 
             labels=c(labels2,labels3),xlab="qualification score" )
title("Model1.6 conditioned on age, yoe, and gender")

"The results generalize for male and female participants,
it does not for the Prefer_not_tell group probably because it
has only 5 participants."

#-----------
#COUNTRY
#Conditioning both on Age and YoE and on Country (indicador variable)
m1.7 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a[country_id] + ba[country_id]*ages + by[country_id]*yoe,
    by[country_id] ~ dnorm( 0 , 0.5 ) ,
    ba[country_id] ~ dnorm( 0 , 0.5 ) ,
    a[country_id] ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = df
) 
precis(m1.7,depth=2)

#labels1 <- paste( "a[" , 1:3 , "]:" , levels(df$country) , sep="" )
labels2 <- paste( "by[" , 1:3  , "]:" , levels(df$country) , sep="" )
labels3 <- paste( "ba[" , 1:3  , "]:" , levels(df$country) , sep="" )

precis_plot( precis( m1.7 , depth=2 , pars=c("ba","by")) , 
             labels=c(labels2,labels3),xlab="qualification score" )
title("Model1.7 conditioned on age, yoe, and country")

"The results generalize for three country groups (US, INDIA, and OTHERS)"