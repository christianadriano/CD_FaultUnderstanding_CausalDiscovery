"
Causal Inference Programming Score Experiment-1
"

library(rethinking)
library(stringr)
library(dplyr)

#Load data
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//load_consent_create_indexes_E1.R")

"Remove participants for whom we do not have years of experience information  (who did not complete the survey)"
df <- df_E1[complete.cases(df_E1),] #left with 485 rows

"Remove people who did no qualify to the test (score<2)"
df <- df[df$qualification_score>=2,] #left with 482 rows

"Outlier in Age. Removing participants who reported to be below 18 years old."
df <- df[df$age>=18,] #removed one, left with 481 rows

"Outlier in Yoe. Removing participants which the difference between age and yoe is less than ten
years-old"
age_minus_yoe <- df$age-df$years_programming
minimum_age_minus_yoe <- age_minus_yoe>=10
df <- df[minimum_age_minus_yoe,] #left with 478 rows

#----------------------
#Rename fields to be easier to place in formulas
df$yoe <- scale(df$years_programming)
df$score <- df$qualification_score #scale(df$qualification_score)
df$ages <- scale(df$age)

boxplot(df$yoe)
table(df$score)
#score:     2    3   4 
#subjects: 328  85  65 

#Model-1.1 only Years of experience
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

m1.2 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a + by*yoe + ba*ages,
    by ~ dnorm( 0 , 0.5 ) ,
    ba ~ dnorm( 0 , 0.5 ) ,
    a ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = df
) 

precis(m1.2)

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

#Direct effect of Age on Score
m1.4 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a + ba*ages,
    ba ~ dnorm( 0 , 0.5 ) ,
    a ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = df
) 
precis(m1.4)
