"
qualification_score versus z1

To which professions this adjustment showed more impact?
How to measure this impact? 
  
  I could standardize the scores (force to be between 0 and 1) and see how
many subjects moved up and down. Subjects who would move down, would be 
the ones who got easier questions correct, but the difficult ones wrong.
"
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tidyr)
library(tidyverse)

#Load only Consent data. No data from tasks, only from demographics and qualification test
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E2.R")
df_E2_ground<- df_consent

scale_01 <- function(x){(x-min(x))/(max(x)-min(x))}

df_consent$qualification_score_scaled <- scale_01(df_consent$qualification_score)
df_consent$z1_scaled <- scale_01(df_consent$z1)


summary(df_consent$qualification_score_scaled)
summary(df_consent$z1_scaled)

df_consent$decreased_score <- df_consent$qualification_score_scaled>df_consent$z1_scaled

df_test <- df_consent[df_consent$decreased_score==TRUE,]

table(df_test$profession)
# Professional            Programmer              Hobbyist      Graduate_Student 
# 138                    19                   152                    82 
# Undergraduate_Student                 Other 
# 152                    37 