" 
Diagnosis the differences in covariate distribution for E2
"

install.packages("tableone")
install.packages("Matching")
install.packages("haven")
library(haven)
library(tableone)
library(Matching)

library(ufs)
library(userfriendlyscience)
library(farff) #all for reading arff file

library(dplyr) #for select and ddply


"Load data with treatment field (isBugCovering) and ground truth (answer correct)"
source("C://Users//Christian//Documents//GitHub//ML_FaultUnderstanding//analysis//descriptive//accuracy//load_apply_ground_truth.R")

df2_ground$explanation_size <- sapply(strsplit(df2_ground$explanation, " "), length);

df2_ground <-
  select(df2_ground,
         'qualification_score',
         'years_programming',
         'experience',
         'confidence',
         'difficulty',
         'duration',
         'explanation_size',
         'isBugCovering');

xvars <- c("qualification_score",
           "years_programming",
           "experience",
           "confidence",
           "difficulty",
           "duration",
           "explanation_size");

table1 <- CreateTableOne(vars=xvars,strata="isBugCovering",data=df2_ground,test=FALSE)
print(table1, smd=TRUE)
