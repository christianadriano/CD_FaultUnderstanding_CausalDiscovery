"Check imbalances in Standardized Mean Differences - SMD
FOR PROFESSIONAL PROGRAMMERS

Treatment: is_fast 
Outcome: adjusted_score (Qualification IRT Score)

"

library(tableone)
library(Matching)
library(MatchIt)

"Load data with treatment field (isBugCovering) and ground truth (answer correct)"
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E2.R")

"Determine the covariates that need to taken into account w.r.t. imbalance:
covariates: age, years_programming, , test_duration, testDuration_membership, profession
effect: adjusted_score"

library(dplyr)

df_selected <- df_selected[df_selected$profession=="Professional",]

df_selected <-
  dplyr::select(df_consent,
                years_programming,
                age,
                test_duration,
                is_fast,
                file_name
                );

xvars <- c("years_programming",
           "age"
          );



# ALL FILE NAMES
raw_table_all <- CreateTableOne(vars=xvars,strata="is_fast",data=df_selected,test=FALSE)
print(raw_table_all, smd=TRUE)

#Data is imbalanced as the SMD is larger than 0.1
# Stratified by is_fast
#                                 FALSE         TRUE          SMD   
# n                               150          1638               
# years_programming (mean (SD))  2.82 (3.19)   5.72 (6.84)   0.544
# age (mean (SD))               27.61 (7.32)  30.46 (8.70)   0.354
# test_duration (mean (SD))      1.31 (0.70)   5.47 (3.26)   1.763
# profession (%)                                             0.898
# Graduate_Student              23 (15.3)    260 (15.9)        
# Hobbyist                      44 (29.3)    440 (26.9)        
# Other                         11 ( 7.3)    101 ( 6.2)        
# Professional                   0 ( 0.0)    417 (25.5)        
# Programmer                    13 ( 8.7)     36 ( 2.2)        
# Undergraduate_Student         59 (39.3)    384 (23.4)    

#HIT01_8
df_file_name <- df_selected[df_selected$file_name=="HIT01_8",]
raw_table <- CreateTableOne(vars=xvars,strata="is_fast",data=df_file_name,test=FALSE)
print(raw_table, smd=TRUE)
# Stratified by is_fast
#                                 FALSE         TRUE          SMD   
# n                                12           131               
# years_programming (mean (SD))  5.50 (5.65)   5.78 (6.22)   0.048
# age (mean (SD))               27.75 (4.49)  29.76 (8.56)   0.294
# test_duration (mean (SD))      1.39 (1.00)   5.69 (3.59)   1.631
# profession (%)                                             1.090
# Graduate_Student               1 ( 8.3)     18 (13.7)        
# Hobbyist                       3 (25.0)     28 (21.4)        
# Other                          2 (16.7)     10 ( 7.6)        
# Professional                   0 ( 0.0)     38 (29.0)        
# Programmer                     2 (16.7)      3 ( 2.3)        
# Undergraduate_Student          4 (33.3)     34 (26.0)  


df_file_name <- df_selected[df_selected$file_name=="HIT02_24",]
raw_table <- CreateTableOne(vars=xvars,strata="is_fast",data=df_file_name,test=FALSE)
print(raw_table, smd=TRUE)
# Stratified by is_fast
#                                 FALSE         TRUE          SMD   
# n                                 9            69               
# years_programming (mean (SD))  2.22 (1.30)   4.88 (5.96)   0.617
# age (mean (SD))               26.78 (5.14)  31.10 (8.97)   0.591
# test_duration (mean (SD))      1.32 (0.88)   5.11 (3.07)   1.677
# profession (%)                                             1.382
# Graduate_Student               1 (11.1)      9 (13.0)        
# Hobbyist                       2 (22.2)     25 (36.2)        
# Other                          0 ( 0.0)      6 ( 8.7)        
# Professional                   0 ( 0.0)     16 (23.2)        
# Programmer                     1 (11.1)      0 ( 0.0)        
# Undergraduate_Student          5 (55.6)     13 (18.8)        

df_file_name <- df_selected[df_selected$file_name=="HIT03_6",]
raw_table <- CreateTableOne(vars=xvars,strata="is_fast",data=df_file_name,test=FALSE)
print(raw_table, smd=TRUE)
    > raw_table_all <- CreateTableOne(vars=xvars,strata="is_fast",data=df_selected,test=FALSE)
  > print(raw_table_all, smd=TRUE)
  # Stratified by is_fast
  #                                FALSE         TRUE          SMD   
  # n                               150          1638               
  # years_programming (mean (SD))  2.82 (3.19)   5.72 (6.84)   0.544
  # age (mean (SD))               27.61 (7.32)  30.46 (8.70)   0.354
  # test_duration (mean (SD))      1.31 (0.70)   5.47 (3.26)   1.763
  # profession (%)                                             0.898
  # Graduate_Student              23 (15.3)    260 (15.9)        
  # Hobbyist                      44 (29.3)    440 (26.9)        
  # Other                         11 ( 7.3)    101 ( 6.2)        
  # Professional                   0 ( 0.0)    417 (25.5)        
  # Programmer                    13 ( 8.7)     36 ( 2.2)        
  # Undergraduate_Student         59 (39.3)    384 (23.4)        
  

  df_file_name <- df_selected[df_selected$file_name=="HIT04_7",]
  raw_table <- CreateTableOne(vars=xvars,strata="is_fast",data=df_file_name,test=FALSE)
  print(raw_table, smd=TRUE)
  # Stratified by is_fast
  #                                 FALSE         TRUE          SMD   
  # n                                57           649               
  # years_programming (mean (SD))  2.73 (2.39)   5.18 (6.65)   0.491
  # age (mean (SD))               27.16 (8.66)  29.97 (8.43)   0.329
  # test_duration (mean (SD))      1.32 (0.65)   5.49 (3.42)   1.690
  # profession (%)                                             0.859
  # Graduate_Student              12 (21.1)    104 (16.0)        
  # Hobbyist                      18 (31.6)    168 (25.9)        
  # Other                          3 ( 5.3)     47 ( 7.2)        
  # Professional                   0 ( 0.0)    156 (24.0)        
  # Programmer                     4 ( 7.0)      8 ( 1.2)        
  # Undergraduate_Student         20 (35.1)    166 (25.6) 

df_file_name <- df_selected[df_selected$file_name=="HIT05_35",]
raw_table <- CreateTableOne(vars=xvars,strata="is_fast",data=df_file_name,test=FALSE)
print(raw_table, smd=TRUE)
# Stratified by is_fast
#                                 FALSE         TRUE          SMD   
# n                                 6            60               
# years_programming (mean (SD))  2.50 (3.73)   8.22 (8.41)   0.879
# age (mean (SD))               25.83 (2.04)  32.12 (9.76)   0.891
# test_duration (mean (SD))      1.62 (1.10)   6.10 (3.66)   1.656
# profession (%)                                             3.310
# Graduate_Student               0 ( 0.0)     14 (23.3)        
# Hobbyist                       0 ( 0.0)     17 (28.3)        
# Other                          3 (50.0)      0 ( 0.0)        
# Professional                   0 ( 0.0)     18 (30.0)        
# Programmer                     1 (16.7)      3 ( 5.0)        
# Undergraduate_Student          2 (33.3)      8 (13.3)  

df_file_name <- df_selected[df_selected$file_name=="HIT06_51",]
raw_table <- CreateTableOne(vars=xvars,strata="is_fast",data=df_file_name,test=FALSE)
print(raw_table, smd=TRUE)
# Stratified by is_fast
#                                 FALSE         TRUE          SMD   
# n                                14           132               
# years_programming (mean (SD))  3.07 (2.64)   6.06 (7.20)   0.552
# age (mean (SD))               30.43 (7.70)  29.69 (7.87)   0.095
# test_duration (mean (SD))      1.28 (0.79)   5.21 (2.81)   1.910
# profession (%)                                             1.047
# Graduate_Student               1 ( 7.1)     17 (12.9)        
# Hobbyist                       5 (35.7)     39 (29.5)        
# Other                          0 ( 0.0)      4 ( 3.0)        
# Professional                   0 ( 0.0)     35 (26.5)        
# Programmer                     2 (14.3)      5 ( 3.8)        
# Undergraduate_Student          6 (42.9)     32 (24.2)    

 
df_file_name <- df_selected[df_selected$file_name=="HIT07_33",]
raw_table <- CreateTableOne(vars=xvars,strata="is_fast",data=df_file_name,test=FALSE)
print(raw_table, smd=TRUE)
# Stratified by is_fast
#                                 FALSE         TRUE          SMD   
# n                                 6            43               
# years_programming (mean (SD))  6.67 (6.62)   6.41 (6.48)   0.040
# age (mean (SD))               31.33 (7.12)  30.60 (6.13)   0.110
# test_duration (mean (SD))      1.36 (0.43)   5.22 (3.77)   1.438
# profession (%)                                             0.995
# Graduate_Student               1 (16.7)      4 ( 9.3)        
# Hobbyist                       3 (50.0)     15 (34.9)        
# Other                          1 (16.7)      4 ( 9.3)        
# Professional                   0 ( 0.0)     13 (30.2)        
# Programmer                     0 ( 0.0)      1 ( 2.3)        
# Undergraduate_Student          1 (16.7)      6 (14.0)   

df_file_name <- df_selected[df_selected$file_name=="HIT08_54",]
raw_table <- CreateTableOne(vars=xvars,strata="is_fast",data=df_file_name,test=FALSE)
print(raw_table, smd=TRUE)
# Stratified by is_fast
#                                 FALSE         TRUE          SMD   
# n                                29           323               
# years_programming (mean (SD))  1.86 (1.87)   5.59 (6.53)   0.776
# age (mean (SD))               26.72 (7.19)  30.43 (8.75)   0.462
# test_duration (mean (SD))      1.25 (0.53)   5.39 (2.90)   1.985
# profession (%)                                             0.937
# Graduate_Student               5 (17.2)     51 (15.8)        
# Hobbyist                      10 (34.5)     79 (24.5)        
# Other                          0 ( 0.0)     19 ( 5.9)        
# Professional                   0 ( 0.0)     77 (23.8)        
# Programmer                     2 ( 6.9)     10 ( 3.1)        
# Undergraduate_Student         12 (41.4)     87 (26.9) 
