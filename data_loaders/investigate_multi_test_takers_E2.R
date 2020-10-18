"
Analyze workers who took the test multiple times = 76
"  
library(farff)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)

path <- "C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data//"
#path <- "C://Users//Christian//Documents//GitHub//DW_Microtasks//output//"
dataset_E2 <- readARFF(paste0(path, "consent_consolidated_Experiment_2.arff"))
df_consent <- data.frame(dataset_E2)
dim(df_consent) 

#Filter-out rows without test data
dim(df_consent[is.na(df_consent$test1),]) #1870 are NA
df_consent <- df_consent[!is.na(df_consent$test1),]
dim(df_consent) #1788 are not NA.

#Analyzing workers who took the test multiple times. am keeping the all the data for people who took the test multiple times, 
#I will take the average of the tests.
#Discover who took multiple times: worker_id's with more than one file_name
unique_workers <- unique(df_consent$worker_id) #1698 unique workers with tests
df_tb <- data.frame(table(df_consent$worker_id))
colnames(df_tb) <- c("worker_id","count")
dim(df_tb[df_tb$count>1,]) #76 workers took the test more than once
multiple_takers <- df_tb[df_tb$count>1,"worker_id"]

df_multi_consent <- df_consent[df_consent$worker_id %in% multiple_takers,]
