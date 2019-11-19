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

"COUNTRY"

df_E1$country <- unlist(lapply(df_E1$country, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}))

df_E1$country <- gsub("UNITED STATES OF AMERICA","US",df_E1$country)
df_E1$country <- gsub("UNITED STATES","US",df_E1$country)
df_E1$country <- gsub("UNITES STATES","US",df_E1$country)
df_E1$country <- gsub("AMERICA","US",df_E1$country)
df_E1$country <- gsub("LOS ANGELES","US",df_E1$country)
df_E1$country <- gsub("DALLAS","US",df_E1$country)
df_E1$country <- gsub("WASHINGTON","US",df_E1$country)
df_E1$country <- gsub("ILLINOIS","US",df_E1$country)
df_E1$country <- gsub("U\\.S\\.A\\.","US",df_E1$country)
df_E1$country <- gsub("U\\.S\\.","US",df_E1$country)
df_E1$country <- gsub("THE US","US",df_E1$country)
df_E1$country <- gsub("USA","US",df_E1$country)
df_E1$country <- gsub("SD","S",df_E1$country)
df_E1$country <- gsub("33","OTHER",df_E1$country)


df_E1$country <- unlist(lapply(df_E1$country, 
                               function(v) {
                                 if(v %in%  c("US","INDIA")) return(v)
                                 else return("OTHER")
                               }))

df_E1$country_labels<- factor(df_E1$country,
                              levels = c("US","INDIA","OTHER")
                              )

df_E1$country_id<- factor(df_E1$country_labels, 
                         levels=levels(df_E1$country_labels),
                         labels = c(1:3)
)
