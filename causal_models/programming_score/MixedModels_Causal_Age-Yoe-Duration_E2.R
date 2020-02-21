" 
Modeling Age and YoE as Poisson Distributions

Using Rethinking package and BRMs package.

Learn how to use the function brm
https://cran.r-project.org/web/packages/brms/vignettes/brms_distreg.html
https://bookdown.org/ajkurz/Statistical_Rethinking_recoded/monsters-and-mixtures.html
"

install.packages("brms") #Bayesian Regression Models using 'Stan'

library(rethinking)
detach(package:rethinking, unload = T)
library(brms)
library(stringr)
library(dplyr)

#Load data
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//load_consent_create_indexes_E2.R")

dim(df_E2) #3657   32

#Remove participants for which we do not have years_programming
df <- df_E2 %>% drop_na(years_programming) #initial 3567, left with 2062 rows

df$yoe <- df$years_programming #scale(df$years_programming)
df$ages <- df$age #scale(df$age)

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
dim(df_E2) #3657   30

#Remove participants for whom we do not have test duration
df <- df %>% drop_na(testDuration_minutes)
dim(df) #1229   32

boxplot(df$testDuration_minutes)
summary(df$testDuration_minutes)

#-------------------------
"AGE > YoE

Results:
m_age_yoe <- a + ba*ages (for every one age year, there is 0.27 yoe)
m_age_yoe.gender 
"

m1.ages.yoe <- quap(
  alist(
    yoe ~ dnorm( mu , sigma ) ,
    mu <- a + ba*ages,
    ba ~ ZIPoisson( 0 , 1 ) ,
    by ~ dnorm( 0 , 1 ) ,
    a ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = df
) 
precis(m1.ages.yoe)


#-------------------------
#Fitting using brms directly
m2.ages.yoe <- brm(
  alist(
    yoe ~ dnorm( mu , sigma ) ,
    mu <- a + ba*ages,
    ba ~ ZIPoisson( 0 , 1 ) ,
    a ~ dnorm(0, 1),
    sigma ~ dexp(1)
  )
) 

brm(data = df, family = zero_inflated_poisson,
    yoe ~ dnorm( mu , sigma ),
    prior = c(prior(normal(0, 1), class = Intercept),
              prior(beta(2, 2), class = zi)),  # the brms default is beta(1, 1)
    cores = 4,
    seed = 11) 