" 
Analyzes the differences in covariate distribution for E2
Creates groups that have balanced distribution based on a controlled variable, which in my case is the
variable named isBugCovering

See the results discussion at the end. It seems that all looks good in terms of covariate balance for E2.

TODO: 
(DONE) add complexity of tasks as a covariate as well (need to add data from Hasltead metric) DONE
- add colunms for IRT and duration_membership to the create_index_E2.R
- Check imbalances for:
   - IRT
   - by duration membership
- Plant how to check imbalances by profession

"

install.packages("tableone")
install.packages("Matching")
install.packages("haven")
library(haven)
library(tableone)
library(Matching)

#all for reading arff file
library(ufs)
library(userfriendlyscience)
library(farff)

library(dplyr) #for select and ddply


"Load data with treatment field (isBugCovering) and ground truth (answer correct)"
path <- "C:\\Users\\Christian\\Documents\\GitHub\\CausalModel_FaultUnderstanding\\data_loaders\\"
source(paste0(path,"load_ground_truth_E2.R"))
source(paste0(path,"create_indexes_E2.R"))

df2_ground <- create_indexes(df_E2_ground)

df2_ground$explanation_size <- sapply(strsplit(df2_ground$explanation, " "), length);

df2_ground <-
  select(df2_ground,
         'qualification_score',
         '',
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

#Without Matching
raw_table <- CreateTableOne(vars=xvars,strata="isBugCovering",data=df2_ground,test=FALSE)
print(raw_table, smd=TRUE)

#                                   Stratified by isBugCovering
#                                   FALSE                 TRUE                  SMD   
# n                                    2080                   500                   
# qualification_score (mean (SD))      4.15 (0.82)           4.23 (0.82)       0.087
# years_programming (mean (SD))        8.50 (8.16)           8.87 (8.84)       0.043
# confidence (mean (SD))               3.43 (1.63)           3.33 (1.56)       0.062
# difficulty (mean (SD))               2.97 (1.28)           3.17 (1.22)       0.161
# duration (mean (SD))            370444.49 (745986.86) 504524.93 (962389.89)  0.156
# explanation_size (mean (SD))        25.47 (27.03)         28.25 (27.42)      0.102

"SMD<0.1 for the metrics of qualification_score, years_programming, and confidence, but close to 0.1
for the remaining. So imbalance is not large."

#Check imbalances by profession


