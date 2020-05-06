"
Load CONSENT data from E2 and create indexes for: profession, qualification_score, file_name, country

"
library(farff)
library(ggplot2)
library(dplyr)
library(tidyr)

#What is the difference between:
# "consent_consolidated_Experiment_2.arff"  [1] 3658   21 has everyone
#  consolidated_Final_Experiment_2.arff" [1] 2580   32 only has the ones who passed the test
#There is a BUG in this file consolidated_Final_Experiment_2.arff 
#this worker_id 3AI7C4g38-9_3 should not have tasks associated!

#path <- "C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data//"
path <- "C://Users//Christian//Documents//GitHub//DW_Microtasks//output//"
dataset_E2 <- readARFF(paste0(path, "consent_consolidated_Experiment_2.arff"))
df_consent <- data.frame(dataset_E2)

#file_path <-
#  "C://Users//Christian//Documents//GitHub//ML_FaultUnderstanding//data//consolidated_Final_Experiment_2.arff"
#df_consolidated_final <-  readARFF(file_path)


#source("C://Users//Christian//Documents//GitHub//ML_FaultUnderstanding//analysis//descriptive//accuracy//load_apply_ground_truth.R")
#data is loaded on df2_ground
#df_consent <- df2_ground

"PROFESSION"
#Convert profession from factor to character 
df_consent$profession <- as.character(df_consent$experience)
#Replaces 'Other...something' for only 'Other'
df_consent[grep("Other",df_consent$profession),"profession"] <- "Other"
#Transform profession as factor again
df_consent$profession <- factor(df_consent$profession, 
                           levels = c("Professional_Developer","Hobbyist",
                                      "Graduate_Student","Undergraduate_Student",
                                      "Other")
)
df_consent$profession_id <- factor(df_consent$profession,
                              levels=levels(df_consent$profession),
                              labels=c(1:5)
)

"QUALIFICATION_SCORE"

#Transform profession as factor again

df_consent$qualification_score_label<- factor(df_consent$qualification_score, 
                                          levels = c(5:0),
                                          labels = c("100%","80%","60%","40%","20%","0%")
                                          )
df_consent$qualification_score_id <- factor(df_consent$qualification_score_label,
                              levels=levels(df_consent$qualification_score_label),
                              labels=c(5:0)
)

"FILE_NAME"

df_consent <- df_consent[df_consent$file_name!="null",]

df_consent$file_name<- factor(df_consent$file_name, 
                         levels = c("HIT01_8","HIT02_24","HIT03_6","HIT04_7",
                                    "HIT05_35","HIT06_51","HIT07_33","HIT08_54")
                        )
df_consent$file_name_id <- factor(df_consent$file_name,
                             levels=levels(df_consent$file_name),
                             labels=c(1:8)
)

"GENDER"

df_consent$gender<- factor(df_consent$gender, 
                      levels = c("Female","Male","Prefer_not_to_tell","Other")
)

df_consent$gender_id<- factor(df_consent$gender, 
                         levels=levels(df_consent$gender),
                         labels = c(1:4)
)

"COUNTRY"


df_consent$country <- unlist(lapply(df_consent$country, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}))

df_consent$country <- gsub("\\bUNITED STATES OF AMERICA\\b","US",df_consent$country)
df_consent$country <- gsub("\\bUNITED STATES\\b","US",df_consent$country)
df_consent$country <- gsub("\\bUNITES STATES\\b","US",df_consent$country)
df_consent$country <- gsub("\\bUNITEDE STATES\\b","US",df_consent$country)
df_consent$country <- gsub("\\bUNITED STAETS\\b","US",df_consent$country)
df_consent$country <- gsub("\\bUNITED\\b","US",df_consent$country)
df_consent$country <- gsub("\\bUNIT\\b","US",df_consent$country)
df_consent$country <- gsub("\\bAMERICA\\b","US",df_consent$country)
df_consent$country <- gsub("LOS ANGELES","US",df_consent$country)
df_consent$country <- gsub("ILLINOIS","US",df_consent$country)
df_consent$country <- gsub("WISCONSIN","US",df_consent$country)
df_consent$country <- gsub("RIVERSIDE","US",df_consent$country)
df_consent$country <- gsub("SAN JOSE CA","US",df_consent$country)
df_consent$country <- gsub("SAN DIEGO","US",df_consent$country)
df_consent$country <- gsub("LOUISIANA","US",df_consent$country)
df_consent$country <- gsub("CALIFORNIA","US",df_consent$country)
df_consent$country <- gsub("ARIZONA","US",df_consent$country)
df_consent$country <- gsub("OHIO","US",df_consent$country)
df_consent$country <- gsub("MARYLAND","US",df_consent$country)
df_consent$country <- gsub("CINCINNATI","US",df_consent$country)
df_consent$country <- gsub("\\bIL\\b","US",df_consent$country)
df_consent$country <- gsub("\\bIN\\b","US",df_consent$country)

df_consent$country <- gsub("U\\.S\\.A\\.","US",df_consent$country)
df_consent$country <- gsub("U\\.S\\.","US",df_consent$country)
df_consent$country <- gsub("U\\.S","US",df_consent$country)
df_consent$country <- gsub("\\bUSA\\b","US",df_consent$country)
df_consent$country <- gsub("GG","OTHER",df_consent$country)
df_consent$country <- gsub("NO","OTHER",df_consent$country)
df_consent$country <- gsub("GURGAON, INDIA","INDIA",df_consent$country)
df_consent$country <- gsub("INDIAN","INDIA",df_consent$country)
df_consent$country <- gsub("MADURAI","INDIA",df_consent$country)
df_consent$country <- gsub("SRILANKA","SRI LANKA",df_consent$country)
df_consent$country <- gsub("ENGLAND","UK",df_consent$country)

df_tb <-  data.frame(table(df_consent$country))
colnames(df_tb) <- c("country","participants")
tribble_cnty <- df_tb %>% group_by(participants)
tribble_cnty <- tribble_cnty %>% summarise(countries_by_participants = n())
tribble_cnty$participants_labels <- as.factor(tribble_cnty$participants)

# barplot(tribble_cnty$countries_by_participants,
#         names.arg = tribble_cnty$participants,
#         xlab="Countries with N participants",
#         ylab="Number of countries",
#         main="Countries by number of participants - E2",
#         ymin=0, ymax=15
#         )
# 
# ggplot(data=tribble_cnty, aes(x=participants_labels, y=countries_by_participants)) +
#   geom_bar(stat="identity", fill="lightgray")+
#   geom_text(aes(label=countries_by_participants), vjust=-0.3, color="black", size=3.0)+
#   theme_minimal()+
#   theme(plot.title = element_text("Helvetica-Narrow", face="plain", colour="black", size=10))+
#   theme(axis.title.x = element_text("Helvetica-Narrow", face="plain", colour="black", size=8))+
#   theme(axis.title.y = element_text("Helvetica-Narrow", face="plain", colour="black", size=8))+
#   theme(axis.text.x  = element_text("Helvetica-Narrow", face="plain", colour="black", size=8))+
#   theme(axis.text.y  = element_text("Helvetica-Narrow", face="plain", colour="black", size=8))+
#   xlab("Number of participants")+
#   ylab("Number of countries")+
#   ggtitle("Countries by number of participants - E2")

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

#DURATION in Minutes
df_consent$testDuration_minutes <- df_consent$testDuration/(1000*60)

#df_consent$duration_minutes <- df_consent$duration/(1000*60)


#Transform isBugCovering as factor again
#df_consent$isBugCovering <- factor(df_consent$isBugCovering, 
#                           levels = c("TRUE","FALSE")
#                          )
#df_consent$isBugCovering_id <- factor(df_consent$isBugCovering,
#                                 levels=levels(df_consent$isBugCovering),
#                                 labels=c(1,0)
#                         )


#Transform answer as factor again
#df_consent$answer <- factor(df_consent$answer, 
#                       levels = c("YES_THERE_IS_AN_ISSUE",
#                                  "NO_THERE_IS_NOT_AN_ISSUE",
#                                  "I_DO_NOT_KNOW")
#                       )
#df_consent$answer_id <- factor(df_consent$answer,
#                          levels=levels(df_consent$answer),
#                          labels=c(1,0,2)
#                    )

#df_consent$explanation.size <- sapply(strsplit(df_consent$explanation, " "), length);


#Transform answer as factor again
#df_consent$isAnswerCorrect_bol <- as.integer(as.logical(df_consent$isAnswerCorrect))

# df_consent$isAnswerCorrect <- factor(df_consent$isAnswerCorrect, 
#                        levels = c("FALSE","TRUE")
# )
# df_consent$isAnswerCorrect_id <- factor(df_consent$isAnswerCorrect,
#                           levels=levels(df_consent$isAnswerCorrect),
#                           labels=c(0,1)
# )

