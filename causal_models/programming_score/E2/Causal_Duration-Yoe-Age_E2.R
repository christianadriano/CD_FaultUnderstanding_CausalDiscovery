" 
Does Yoe and Age affect duration of tests?

TODO: test across countries and gender
"

library(rethinking)
library(stringr)
library(dplyr)

#Load data
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//load_consent_create_indexes_E2.R")

dim(df_E2) #3657   32

#Remove participants for which we do not have years_programming
df <- df_E2 %>% drop_na(years_programming) #initial 3567, left with 2062 rows

df$yoe <- scale(df$years_programming)
df$ages <- scale(df$age)

dim(df) #2062 32 Reduced 1595 rows

#-----------------
"OUTLIERS in DURATION"
"Code comprehension studies show that a programmer takes from 12 to 24 seconds is also the 
average minimum time to read one line of code."

"The lower cut to the minimum time to read all 5 questions and corresponding lines of code
in the qualification test. 
 Since the test has 5 questions, each question 
requires the inspection of one line of code, that would require the programmer from 60s to 120s.
We chose 120s (2 min) as the minimum time-effort one need to read and answer all 5 questions"

#The upper cut corresponds to 10 times the minimum. We choose 24s, so 60 min.

df <- df[df$testDuration_minutes<=15 & df$testDuration_minutes>=1 ,]
dim(df) # 1853   32

#Remove participants for whom we do not have test duration
df <- df %>% drop_na(testDuration_minutes)
dim(df) #1229   32

boxplot(df$testDuration_minutes)
summary(df$testDuration_minutes)

#-------------------------
"YOE + AGE > Duration
Does an increase in age relates to an increase in duration
Does an increase in yoe relates to an increase in duration
Does this happen across different categories:
gender, profession, country?

Results:
m_age_yoe <- a + ba*ages (for every one age year, there is 0.27 yoe)
m_age_yoe.gender 
"

m1.ages.yoe <- quap(
  alist(
    testDuration_minutes ~ dnorm( mu , sigma ) ,
    mu <- a + ba*ages + by*yoe,
    ba ~ dnorm( 0 , 1 ) ,
    by ~ dnorm( 0 , 1 ) ,
    a ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = df
) 
precis(m1.ages.yoe)
#       mean   sd 5.5% 94.5%  (duration bounds [1,15] min)
# ba    0.36 0.15 0.12  0.61
# by    0.24 0.09 0.09  0.39
# a     5.04 0.09 4.90  5.18
# sigma 3.10 0.06 3.00  3.19

m1.yoe <- quap(
  alist(
    testDuration_minutes ~ dnorm( mu , sigma ) ,
    mu <- a + by*yoe,
    by ~ dnorm( 0 , 1 ) ,
    a ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = df
) 
precis(m1.yoe)
#       mean   sd 5.5% 94.5%  (duration bounds [1,15] min)
# by    0.34 0.08 0.20  0.47
# a     5.02 0.09 4.88  5.16
# sigma 3.10 0.06 3.00  3.20

m1.ages <- quap(
  alist(
    testDuration_minutes ~ dnorm( mu , sigma ) ,
    mu <- a + ba*ages,
    ba ~ dnorm( 0 , 1 ) ,
    a ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = df
) 
precis(m1.ages)
#       mean   sd 5.5% 94.5% (duration bounds [1,15] min)
# ba    0.54 0.14 0.31  0.76
# a     5.06 0.09 4.92  5.20
# sigma 3.10 0.06 3.00  3.20

compare(m1.yoe,m1.ages,m1.ages.yoe, func=PSIS)
#               PSIS    SE dPSIS  dSE pPSIS weight
# m1.ages     6051.6 64.42     0   NA   3.7   0.46
# m1.ages.yoe 6052.7 64.87     1 2.15   4.6   0.27
# m1.yoe      6052.7 64.78     1 4.05   3.6   0.27
compare(m1.yoe,m1.ages,m1.ages.yoe, func=WAIC)
#               WAIC    SE dWAIC  dSE pWAIC weight
# m1.ages     6051.6 64.49   0.0   NA   3.5   0.46
# m1.yoe      6052.5 64.77   0.9 4.05   3.4   0.29
# m1.ages.yoe 6052.8 64.85   1.2 1.95   4.7   0.25

#------------------------------------------
#PROFESSIONS

m1.ages.yoe.prof <- quap(
  alist(
    testDuration_minutes ~ dnorm( mu , sigma ) ,
    mu <- a[profession_id] + ba[profession_id]*ages + by[profession_id]*yoe,
    a[profession_id] ~ dnorm( 0 , 1 ) ,
    ba[profession_id] ~ dnorm( 0 , 1 ) ,
    by[profession_id] ~ dnorm( 0 , 1 ) ,
    sigma ~ dexp(1)
  ), data = df
) 
precis(m1.ages.yoe.prof, depth = 2)
#Does not generalize for all professions.
#Coefficients ba and by do not cross zero only for hobbyists

m1.yoe.prof <- quap(
  alist(
    testDuration_minutes ~ dnorm( mu , sigma ) ,
    mu <- a[profession_id] + by[profession_id]*yoe,
    by[profession_id] ~ dnorm( 0 , 1 ) ,
    a[profession_id] ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = df
) 
precis(m1.yoe.prof, depth=2)
#Only for Hobbyists, Undergrads, and Others it does not cross zero

m1.ages.prof <- quap(
  alist(
    testDuration_minutes ~ dnorm( mu , sigma ) ,
    mu <- a[profession_id] + ba[profession_id]*ages,
    ba[profession_id] ~ dnorm( 0 , 1 ) ,
    a[profession_id] ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = df
) 
precis(m1.ages.prof, depth=2)
#Similar results across professions. Coefficient ba cross zero for all professions.
#Only for Hobbyists and Others the coefficient ba does not cross zero

" The conclusion is that the following models can be used to 
explain the causal effect of yoe and ages on duration for the following 
professions:
Hobbyists: m1.ages, m1.yoe, m1.ages.yoe
Undergrad: m1.yoe
Others: m1.ages, m1.yoe,
Professionals: none
Graduates: none

"
