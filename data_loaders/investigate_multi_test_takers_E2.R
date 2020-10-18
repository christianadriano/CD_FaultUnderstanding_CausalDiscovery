"
Analyze workers who took the test multiple times = 76

Worker IDs with more than one test
270ie-2C-4c-550:97ai0I-8C871_3
1198eA8G-7e7-7-3:120Gc-7G5G4-5-4_3

"  
library(farff)
library(readr)
library(dplyr)
library(tidyr)

path <- "C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data//"
dataset_E2 <- readARFF(paste0(path, "consent_consolidated_Experiment_2.arff"))
df_consent <- data.frame(dataset_E2)
dim(df_consent) 

#Filter-out rows without test data
dim(df_consent[is.na(df_consent$test1),]) #1870 are NA
df_consent <- df_consent[!is.na(df_consent$test1),]
dim(df_consent) #1788 are not NA.

#Analyzing workers who took the test multiple times. am keeping the all the data for people who took the test multiple times, 
#Discover who took multiple times: worker_id's with more than one file_name
unique_workers <- unique(df_consent$worker_id) #1698 unique workers with tests
length(unique_workers) #1698 unique
df_tb <- data.frame(table(df_consent$worker_id))
colnames(df_tb) <- c("worker_id","count")
dim(df_tb[df_tb$count>1,]) #76 workers took the test more than once
multiple_takers <- df_tb[df_tb$count>1,"worker_id"]

df_multi_consent <- df_consent[df_consent$worker_id %in% multiple_takers,]


#Another way to do it.
df_select <- select(df_consent,worker_id,file_name)
df_a <- aggregate(df_select,by=list(df_select$worker_id),FUN=length)
df_multiple <- df_a[df_a$worker_id>1,]
dim(df_multiple) #76
