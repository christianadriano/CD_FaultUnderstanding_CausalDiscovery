"Check imbalances in Standardized Mean Differences - SMD


Treatment: Qualification Score (IRT Score)
Analyze imbalance for each Task (HIT01 to HIT08)
Analyze imbalance across professions
plotting results with Cobalt
https://cran.rstudio.com/web/packages/cobalt/vignettes/cobalt_A4_love.plot.html

"

"Load data with treatment field (isBugCovering) and ground truth (answer correct)"
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E2.R")

"Determine the covariates that need to taken into account w.r.t. imbalance:
covariates: age, years_programming, , test_duration, testDuration_membership, profession
effect: adjusted_score"

library(dplyr)

df_selected <-
  dplyr::select(df_consent,
                profession, 
                years_programming,
                age,
                test_duration,
                testDuration_fastMembership
                );

xvars <- c("years_programming",
           "age",
           "test_duration",
           "profession"
          );


library(tableone)
library(Matching)
#Now load the lalonde data (which is in the MatchIt package):
library(MatchIt)
#Without Matching

# ALL FILE NAMES
raw_table_all <- CreateTableOne(vars=xvars,strata="testDuration_fastMembership",data=df_selected,test=FALSE)
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
df_file_name <- df_selected[df_selected$file_name=="HIT01_8",]
raw_table <- CreateTableOne(vars=xvars,strata="testDuration_fastMembership",data=df_file_name,test=FALSE)
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


df_file_name <- df_selected[df_selected$file_name=="HIT02_24",]
raw_table <- CreateTableOne(vars=xvars,strata="testDuration_fastMembership",data=df_file_name,test=FALSE)
print(raw_table, smd=TRUE)
#                                 Stratified by isBugCovering
#                                   TRUE           FALSE         SMD 
# n                                  22            26               
# years_programming (mean (SD))   18.27 (13.18) 13.94 (11.04)  0.356
# qualification_score (mean (SD))  4.09 (0.81)   4.00 (0.94)   0.104
# volume_Halstead (mean (SD))     52.84 (30.27) 48.94 (6.83)   0.178
# profession (%)                                               0.612
# Professional_Developer           8 (36.4)     16 (61.5)        
# Hobbyist                         8 (36.4)      4 (15.4)        
# Graduate_Student                 0 ( 0.0)      0 ( 0.0)        
# Undergraduate_Student            4 (18.2)      5 (19.2)        
# Other                            2 ( 9.1)      1 ( 3.8) 

df_file_name <- df_selected[df_selected$file_name=="HIT03_6",]
raw_table <- CreateTableOne(vars=xvars,strata="testDuration_fastMembership",data=df_file_name,test=FALSE)
print(raw_table, smd=TRUE)
#                                 Stratified by isBugCovering
#                                   TRUE           FALSE         SMD 
# n                                   45            155               
# years_programming (mean (SD))    10.42 (8.53)    9.01 (8.02)   0.171
# qualification_score (mean (SD))   4.18 (0.81)    4.15 (0.75)   0.038
# volume_Halstead (mean (SD))     488.84 (563.39) 90.72 (97.40)  0.985
# profession (%)                                                 0.359
# Professional_Developer           28 (62.2)      77 (49.7)        
# Hobbyist                          7 (15.6)      28 (18.1)        
# Graduate_Student                  2 ( 4.4)      21 (13.5)        
# Undergraduate_Student             6 (13.3)      22 (14.2)        
# Other                             2 ( 4.4)       7 ( 4.5)  

df_file_name <- df_selected[df_selected$file_name=="HIT04_7",]
raw_table <- CreateTableOne(vars=xvars,strata="testDuration_fastMembership",data=df_file_name,test=FALSE)
print(raw_table, smd=TRUE)
#                                 Stratified by isBugCovering
#                                   TRUE           FALSE         SMD 
# n                                   73             441                
# years_programming (mean (SD))     8.23 (10.54)    7.61 (8.72)    0.064
# qualification_score (mean (SD))   4.21 (0.80)     4.14 (0.77)    0.085
# volume_Halstead (mean (SD))     254.61 (300.61) 160.44 (173.94)  0.383
# profession (%)                                                   0.134
# Professional_Developer           22 (30.1)      147 (33.3)         
# Hobbyist                         13 (17.8)       84 (19.0)         
# Graduate_Student                 13 (17.8)       70 (15.9)         
# Undergraduate_Student            17 (23.3)      106 (24.0)         
# Other                             8 (11.0)       34 ( 7.7)  

df_file_name <- df_selected[df_selected$file_name=="HIT05_35",]
raw_table <- CreateTableOne(vars=xvars,strata="testDuration_fastMembership",data=df_file_name,test=FALSE)
print(raw_table, smd=TRUE)
#                                 Stratified by isBugCovering
#                                   TRUE           FALSE         SMD 
# n                                   28            56               
# years_programming (mean (SD))    10.29 (8.18)  10.34 (8.06)   0.007
# qualification_score (mean (SD))   4.14 (0.89)   4.14 (0.88)  <0.001
# volume_Halstead (mean (SD))     114.95 (34.26) 95.03 (40.20)  0.533
# profession (%)                                                0.067
# Professional_Developer           17 (60.7)     33 (58.9)        
# Hobbyist                          3 (10.7)      6 (10.7)        
# Graduate_Student                  2 ( 7.1)      5 ( 8.9)        
# Undergraduate_Student             2 ( 7.1)      4 ( 7.1)        
# Other                             4 (14.3)      8 (14.3) 

df_file_name <- df_selected[df_selected$file_name=="HIT06_51",]
raw_table <- CreateTableOne(vars=xvars,strata="testDuration_fastMembership",data=df_file_name,test=FALSE)
print(raw_table, smd=TRUE)
#                                 Stratified by isBugCovering
#                                   TRUE           FALSE         SMD 
# n                                   31            145               
# years_programming (mean (SD))    11.58 (10.39)   8.83 (8.35)   0.292
# qualification_score (mean (SD))   4.52 (0.68)    4.18 (0.75)   0.471
# volume_Halstead (mean (SD))     408.51 (387.85) 57.39 (68.17)  1.261
# profession (%)                                                 0.292
# Professional_Developer           13 (41.9)      59 (40.7)        
# Hobbyist                          7 (22.6)      28 (19.3)        
# Graduate_Student                  6 (19.4)      19 (13.1)        
# Undergraduate_Student             3 ( 9.7)      24 (16.6)        
# Other                             2 ( 6.5)      15 (10.3)  
df_file_name <- df_selected[df_selected$file_name=="HIT07_33",]
raw_table <- CreateTableOne(vars=xvars,strata="testDuration_fastMembership",data=df_file_name,test=FALSE)
print(raw_table, smd=TRUE)
#                                 Stratified by isBugCovering
#                                   TRUE           FALSE         SMD 
# n                                  21            48               
# years_programming (mean (SD))    9.48 (8.91)  10.17 (7.00)   0.086
# qualification_score (mean (SD))  4.52 (0.75)   4.27 (0.74)   0.340
# volume_Halstead (mean (SD))     71.11 (45.85) 88.17 (54.17)  0.340
# profession (%)                                               0.516
# Professional_Developer           7 (33.3)     26 (54.2)        
# Hobbyist                         9 (42.9)     14 (29.2)        
# Graduate_Student                 1 ( 4.8)      2 ( 4.2)        
# Undergraduate_Student            1 ( 4.8)      0 ( 0.0)        
# Other                            3 (14.3)      6 (12.5)  

df_file_name <- df_selected[df_selected$file_name=="HIT08_54",]
raw_table <- CreateTableOne(vars=xvars,strata="testDuration_fastMembership",data=df_file_name,test=FALSE)
print(raw_table, smd=TRUE)
#                                 Stratified by isBugCovering
#                                   TRUE           FALSE         SMD 
# n                                   31             210                
# years_programming (mean (SD))    11.55 (13.08)    9.16 (8.54)    0.217
# qualification_score (mean (SD))   4.13 (0.72)     4.03 (0.83)    0.129
# volume_Halstead (mean (SD))     689.43 (746.43) 112.97 (146.47)  1.072
# profession (%)                                                   0.389
# Professional_Developer           14 (45.2)       77 (36.7)         
# Hobbyist                          4 (12.9)       51 (24.3)         
# Graduate_Student                  3 ( 9.7)       20 ( 9.5)         
# Undergraduate_Student             4 (12.9)       38 (18.1)         
# Other                             6 (19.4)       24 (11.4)  

