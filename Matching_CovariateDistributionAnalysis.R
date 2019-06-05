" 
Diagnosis the differences in covariate distribution for E2
"

install.packages("tableone")
install.packages("Matching")
library(tableone)
library(Matching)

library(ufs)
library(userfriendlyscience)
library(farff) #all for reading arff file

library(dplyr) #for select and ddply



file_path <-
  "C://Users//Christian//Documents//GitHub//ML_FaultUnderstanding//data//consolidated_Final_Experiment_2.arff"
df2 <-  readARFF(file_path);
df2$explanation_size <- sapply(strsplit(df2$explanation, " "), length);

df2 <-
  select(df2,
         'qualification_score',
         'years_programming',
         'experience',
         'confidence',
         'difficulty',
         'duration',
         'explanation_size');

xvars <- c("qualification_score",
           "years_programming",
           "experience",
           "confidence",
           "difficulty",
           "duration",
           "explanation_size");

table1 <- CreateTableOne(vars=xvars,strata="treatment",data=df2,test=FALSE)
