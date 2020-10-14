"
Relabel Others who are software developers

Keywords: IT, developer, programmer, computer, Tech, Technician, software, computer, QA, DBA, Data, 

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
dim(df_consent) #3658

#remove rows without profession information
df_consent <- df_consent[!is.na(df_consent$experience),] #left with 2463

pattern <- "it|developer|programmer|computer|tech|technician|software|computer|QA|DBA|Data"

df_others <- df_consent[grep("other",tolower(df_consent$experience)),]

#create three groups. All other, other developer, other not_developer,
df_consent[(grep(pattern,tolower(df_consent$experience))),"experience"] <- "Professional_Developer"


#Mean qualification score of Other programmers
hist(df_consent[(grep(pattern,tolower(df_consent$experience))),"qualification_score"])

df_consent[(grep(pattern,tolower(df_consent$experience))),"experience"] <- "Professional_Developer"
