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
path <- "C://Users//Christian//Documents//GitHub//Complexity_Metrics//output//"
dataset_E2 <- read.csv(str_c(path, "merged_tasks_complexity_E2.csv"))
df_E2 <- data.frame(dataset_E2)


# standardize variables = (zero centered, standard deviation one)
df_E2$yoe <- scale(df_E2$years_programming)
df_E2$score <- scale(df_E2$qualification_score)

#create indexes for each profession

#Replaces 'Other...something' for only 'Other'
df_E2[grep("Other",df_E2$profession),"prf"] <- "Other"

df_E2$prf <- case_when(
  df_E2$profession=="Professional_Developer" ~ 5,
  df_E2$profession=="Hobbyist" ~ 4,
  df_E2$profession=="Graduate_Student" ~ 3,
  df_E2$profession=="Undergraduate_Student" ~ 2,
  df_E2$profession=="Other" ~ 1,
)

#Model-1 No interaction, only addivite effects
m1 <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a + by*yoe bp*profession[i],
    a ~ dnorm( 0 , 1 ) ,
    by ~ dnorm( 0 , 0.5 ) , #only postive relation between yoe and score
    sigma ~ dexp(1)
  ), data = df_E2
) 

