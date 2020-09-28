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


library(tableone)
library(Matching)
#Now load the lalonde data (which is in the MatchIt package):
library(MatchIt)
#Without Matching

# ALL FILE NAMES
raw_table_all <- CreateTableOne(vars=xvars,strata="isBugCovering",data=df_E2_ground,test=FALSE)
print(raw_table_all, smd=TRUE)

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

#HIT01_8
df_file_name <- df_E2_ground[df_E2_ground$file_name=="HIT01_8",]
raw_table <- CreateTableOne(vars=xvars,strata="isBugCovering",data=df_file_name,test=FALSE)
print(raw_table, smd=TRUE)
#                                 Stratified by isBugCovering
#                                   TRUE           FALSE         SMD   
# n                                   27           103               
# years_programming (mean (SD))    11.93 (11.29)  9.16 (9.37)   0.267
# qualification_score (mean (SD))   4.15 (0.77)   4.17 (0.79)   0.022
# volume_Halstead (mean (SD))     101.74 (44.29) 95.70 (74.35)  0.099
# profession (%)                                                0.437
# Professional_Developer           11 (40.7)     39 (37.9)        
# Hobbyist                          4 (14.8)     14 (13.6)        
# Graduate_Student                  5 (18.5)     12 (11.7)        
# Undergraduate_Student             4 (14.8)     32 (31.1)        
# Other                             3 (11.1)      6 ( 5.8)  


df2_fileName <- df_E2_ground[df_E2_ground$file_name=="HIT02_4",]
