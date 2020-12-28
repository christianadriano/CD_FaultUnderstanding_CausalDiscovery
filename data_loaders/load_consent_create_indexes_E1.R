"
Load data from E1 and create indexes for qualification_score, gender
"

library(farff)
library(ggplot2)
library(dplyr)

#--------------------------
"LOAD FILES"
path <- "C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data//"
dataset_E1 <- readARFF(paste0(path,"consent_consolidated_Experiment_1.arff"))
df_consent <- data.frame(dataset_E1)
dim(df_consent) #4776   16


#------------------------
"MISSING DATA"

#Filter-out rows without test data
dim(df_consent[is.na(df_consent$test1),]) #1077 are NA
df_consent <- df_consent[!is.na(df_consent$test1),]
dim(df_consent) #3699 are not NA.



#-----------------------
"QUALIFICATION_SCORE"


df_consent$qualification_score_label<- factor(df_consent$qualification_score, 
                                          levels = c(4:0),
                                          labels = c("100%","75%","50%","25%","0%")
                                          )
df_consent$qualification_score_id <- factor(df_consent$qualification_score_label,
                                      levels=levels(df_consent$qualification_score_label),
                                      labels=c(4:0)
                                      )
#-----------------------
"GENDER"

df_consent$gender<- factor(df_consent$gender, 
                      levels = c("Female","Male","Prefer_not_to_tell")
                      )

df_consent$gender_id<- factor(df_consent$gender, 
                         levels=levels(df_consent$gender),
                         labels = c(1:3)
                        )
#-----------------------
"COUNTRY"

df_consent$country <- unlist(lapply(df_consent$country, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}))

df_consent$country <- gsub("UNITED STATES OF AMERICA","US",df_consent$country)
df_consent$country <- gsub("UNITED STATES","US",df_consent$country)
df_consent$country <- gsub("UNITES STATES","US",df_consent$country)
df_consent$country <- gsub("AMERICA","US",df_consent$country)
df_consent$country <- gsub("LOS ANGELES","US",df_consent$country)
df_consent$country <- gsub("DALLAS","US",df_consent$country)
df_consent$country <- gsub("WASHINGTON","US",df_consent$country)
df_consent$country <- gsub("ILLINOIS","US",df_consent$country)
df_consent$country <- gsub("U\\.S\\.A\\.","US",df_consent$country)
df_consent$country <- gsub("U\\.S\\.","US",df_consent$country)
df_consent$country <- gsub("THE US","US",df_consent$country)
df_consent$country <- gsub("USA","US",df_consent$country)
df_consent$country <- gsub("SD","S",df_consent$country)
df_consent$country <- gsub("33","OTHER",df_consent$country)


df_consent$country <- unlist(lapply(df_consent$country, 
                               function(v) {
                                 if(v %in%  c("US","INDIA")) return(v)
                                 else return("OTHER")
                               }))

df_consent$country_labels<- factor(df_consent$country,
                              levels = c("US","INDIA","OTHER")
                              )

df_consent$country_id<- factor(df_consent$country_labels, 
                         levels=levels(df_consent$country_labels),
                         labels = c(1:3)
)

#-----------------------
#DURATION of TEST
#Convert to minutes
df_consent$test_duration <- df_consent$test_duration/(1000*60)

print(paste0("Loaded ",dim(df_consent)[1], " rows."," Results are in df_consent"))

#---------------------
#END
#---------------------

print("Results in df_consent")