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
