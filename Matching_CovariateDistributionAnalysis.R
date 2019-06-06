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
        
         'confidence',
         'difficulty',
         'duration',
         'explanation_size',
         'isBugCovering');

# 'experience',

xvars <- c("qualification_score",
           "years_programming",
           "confidence",
           "difficulty",
           "duration",
           "explanation_size");

table1 <- CreateTableOne(vars=xvars,strata="isBugCovering",data=df2_ground,test=FALSE)
print(table1, smd=TRUE)

greedymatch <- Match(Tr=df2_ground$isBugCovering, M=1, X=df2_ground[xvars])
matched <- df2_ground[unlist(greedymatch[c("index.treated","index.control")]),]

matchedTab1 <- CreateTableOne(vars=xvars, strata="isBugCovering", data=matched, test=FALSE)
print(matchedTab1, smd=TRUE)
