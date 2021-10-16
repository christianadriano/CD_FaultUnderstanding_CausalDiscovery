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

source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//remove_outliers_E2.R")

#Load data
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E2.R")
df_consent <- load_consent_create_indexes()
dim(df_consent) #1788   29

#Remove participants for which we do not have years_programming
df <- df_consent %>% drop_na(years_programming)

df$yoe <- df$years_programming #scale(df$years_programming)
df$ages <- df$age #scale(df$age)

dim(df) #1788   31

#-----------------

df <- remove_outliers_test_duration(df)
dim(df) #1691   29

boxplot(df$test_duration)
summary(df$test_duration)

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