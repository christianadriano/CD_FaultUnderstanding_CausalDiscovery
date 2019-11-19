"
Load CONSENT data from E2 and create indexes for: profession, qualification_score, file_name, country

"
library(farff)
library(ggplot2)
library(dplyr)

path <- "C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data//"
dataset_E2 <- readARFF(paste0(path, "consent_consolidated_Experiment_2.arff"))
df_E2 <- data.frame(dataset_E2)

"PROFESSION"
#Convert profession from factor to character 
df_E2$profession <- as.character(df_E2$experience)
#Replaces 'Other...something' for only 'Other'
df_E2[grep("Other",df_E2$profession),"profession"] <- "Other"
#Transform profession as factor again
df_E2$profession <- factor(df_E2$profession, 
                           levels = c("Professional_Developer","Hobbyist",
                                      "Graduate_Student","Undergraduate_Student",
                                      "Other")
)
df_E2$profession_id <- factor(df_E2$profession,
                              levels=levels(df_E2$profession),
                              labels=c(1:5)
)

"QUALIFICATION_SCORE"

#Transform profession as factor again


df_E2$qualification_score_label<- factor(df_E2$qualification_score, 
                                          levels = c(5:3),
                                          labels = c("100%","80%","60%")
                                          )
df_E2$qualification_score_id <- factor(df_E2$qualification_score_label,
                              levels=levels(df_E2$qualification_score_label),
                              labels=c(3:1)
)

"FILE_NAME"

df_E2$file_name<- factor(df_E2$file_name, 
                         levels = c("HIT01_8","HIT02_24","HIT03_6","HIT04_7",
                                    "HIT05_35","HIT06_51","HIT07_33","HIT08_54")
                        )
df_E2$file_name_id <- factor(df_E2$file_name,
                             levels=levels(df_E2$file_name),
                             labels=c(1:8)
)

"COUNTRY"



"Buggy not Buggy"
