"Check imbalances in Standardized Mean Differences - SMD

plotting results with Cobalt
https://cran.rstudio.com/web/packages/cobalt/vignettes/cobalt_A4_love.plot.html

Treatment: Bug or NoBug
Analyze imbalance for each Task (HIT01 to HIT08)
"

"Load data with treatment field (isBugCovering) and ground truth (answer correct)"
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_ground_truth_E2.R")
#summary(df_E2_ground)

source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//create_indexes_E2.R")

df_E2_ground<- run(df_E2_ground)

"Determine the covariates that need to taken into account w.r.t. imbalance.
The covariates are the ones that are confounders of the treatment (bug/nobug) and the 
effect (accuracy of the task)"

#see DAG in file DAG_ExogenousIntervention.R

"The covariates that can imbalance the intervention are:
- qualification_score
- profession
- volume_Halstead

We could evaluate balance by each file_name, which is a separate experiment.
"

library(dplyr)

df_selected <-
  dplyr::select(df_E2_ground,profession, 
                years_programming,
                qualification_score,
                volume_Halstead,
                profession,
                isBugCovering,
                file_name
                );

xvars <- c("years_programming",
           "qualification_score",
           "volume_Halstead",
           "profession"
          );

df2_fileName <- df_E2_ground[df_E2_ground$file_name=="HIT01_8",]

library(tableone)
library(Matching)
#Now load the lalonde data (which is in the MatchIt package):
library(MatchIt)
#Without Matching
raw_table <- CreateTableOne(vars=xvars,strata="isBugCovering",data=df_E2_ground,test=FALSE)
print(raw_table, smd=TRUE)

#Data is imbalanced as the SMD is larger than 0.1
# Stratified by isBugCovering
#                                    TRUE            FALSE           SMD   
# n                                  278            1184                
# years_programming (mean (SD))    10.78 (10.71)    8.72 (8.63)    0.212
# qualification_score (mean (SD))   4.23 (0.79)     4.13 (0.79)    0.127
# volume_Halstead (mean (SD))     299.43 (437.96) 116.17 (137.63)  0.565
# profession (%)                                                   0.149
# Professional_Developer          120 (43.2)      474 (40.0)         
# Hobbyist                         55 (19.8)      229 (19.3)         
# Graduate_Student                 32 (11.5)      149 (12.6)         
# Undergraduate_Student            41 (14.7)      231 (19.5)         
# Other                            30 (10.8)      101 ( 8.5)    
