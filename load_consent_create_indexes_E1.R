"
Load data from E1 and create indexes for qualification_score, gender
"

library(farff)
library(ggplot2)
library(dplyr)

path <- "C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data//"
dataset_E1 <- readARFF(paste0(path,"consent_consolidated_Experiment_1.arff"))
df_E1 <- data.frame(dataset_E1)

head(df_E1)

df <- unique(df)


"QUALIFICATION_SCORE"


df_E1$qualification_score_label<- factor(df_E1$qualification_score, 
                                          levels = c(4:0),
                                          labels = c("100%","75%","50%","25%","0%")
                                          )
df_E1$qualification_score_id <- factor(df_E1$qualification_score_label,
                                      levels=levels(df_E1$qualification_score_label),
                                      labels=c(4:0)
                                      )

"GENDER"

df_E1$gender<- factor(df_E1$gender, 
                      labels = c("Female","Male","Prefer_not_to_tell")
                      )

df_E1$gender_id<- factor(df_E1$gender, 
                         levels=levels(df_E1$gender),
                         labels = c(1:3)
                        )

