"
Load CONSENT data from E2 and create indexes for: profession, qualification_score, file_name, country

"
library(farff)
library(ggplot2)
library(dplyr)
library(tidyr)

#path <- "C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data//"
#dataset_E2 <- readARFF(paste0(path, "consent_consolidated_Experiment_2.arff"))
#df_E2 <- data.frame(dataset_E2)

source("C://Users//Christian//Documents//GitHub//ML_FaultUnderstanding//analysis//descriptive//accuracy//load_apply_ground_truth.R")
#data is loaded on df2_ground

df_E2 <- df2_ground

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
                                          levels = c(5:0),
                                          labels = c("100%","80%","60%","40%","20%","0%")
                                          )
df_E2$qualification_score_id <- factor(df_E2$qualification_score_label,
                              levels=levels(df_E2$qualification_score_label),
                              labels=c(5:0)
)

"FILE_NAME"

df_E2 <- df_E2[df_E2$file_name!="null",]

df_E2$file_name<- factor(df_E2$file_name, 
                         levels = c("HIT01_8","HIT02_24","HIT03_6","HIT04_7",
                                    "HIT05_35","HIT06_51","HIT07_33","HIT08_54")
                        )
df_E2$file_name_id <- factor(df_E2$file_name,
                             levels=levels(df_E2$file_name),
                             labels=c(1:8)
)

"GENDER"

df_E2$gender<- factor(df_E2$gender, 
                      levels = c("Female","Male","Prefer_not_to_tell","Other")
)

df_E2$gender_id<- factor(df_E2$gender, 
                         levels=levels(df_E2$gender),
                         labels = c(1:4)
)

"COUNTRY"


df_E2$country <- unlist(lapply(df_E2$country, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}))

df_E2$country <- gsub("\\bUNITED STATES OF AMERICA\\b","US",df_E2$country)
df_E2$country <- gsub("\\bUNITED STATES\\b","US",df_E2$country)
df_E2$country <- gsub("\\bUNITES STATES\\b","US",df_E2$country)
df_E2$country <- gsub("\\bUNITEDE STATES\\b","US",df_E2$country)
df_E2$country <- gsub("\\bUNITED STAETS\\b","US",df_E2$country)
df_E2$country <- gsub("\\bUNITED\\b","US",df_E2$country)
df_E2$country <- gsub("\\bUNIT\\b","US",df_E2$country)
df_E2$country <- gsub("\\bAMERICA\\b","US",df_E2$country)
df_E2$country <- gsub("LOS ANGELES","US",df_E2$country)
df_E2$country <- gsub("ILLINOIS","US",df_E2$country)
df_E2$country <- gsub("WISCONSIN","US",df_E2$country)
df_E2$country <- gsub("RIVERSIDE","US",df_E2$country)
df_E2$country <- gsub("SAN JOSE CA","US",df_E2$country)
df_E2$country <- gsub("SAN DIEGO","US",df_E2$country)
df_E2$country <- gsub("LOUISIANA","US",df_E2$country)
df_E2$country <- gsub("CALIFORNIA","US",df_E2$country)
df_E2$country <- gsub("ARIZONA","US",df_E2$country)
df_E2$country <- gsub("OHIO","US",df_E2$country)
df_E2$country <- gsub("MARYLAND","US",df_E2$country)
df_E2$country <- gsub("CINCINNATI","US",df_E2$country)
df_E2$country <- gsub("\\bIL\\b","US",df_E2$country)
df_E2$country <- gsub("\\bIN\\b","US",df_E2$country)

df_E2$country <- gsub("U\\.S\\.A\\.","US",df_E2$country)
df_E2$country <- gsub("U\\.S\\.","US",df_E2$country)
df_E2$country <- gsub("U\\.S","US",df_E2$country)
df_E2$country <- gsub("\\bUSA\\b","US",df_E2$country)
df_E2$country <- gsub("GG","OTHER",df_E2$country)
df_E2$country <- gsub("NO","OTHER",df_E2$country)
df_E2$country <- gsub("GURGAON, INDIA","INDIA",df_E2$country)
df_E2$country <- gsub("INDIAN","INDIA",df_E2$country)
df_E2$country <- gsub("MADURAI","INDIA",df_E2$country)
df_E2$country <- gsub("SRILANKA","SRI LANKA",df_E2$country)
df_E2$country <- gsub("ENGLAND","UK",df_E2$country)

df_tb <-  data.frame(table(df_E2$country))
colnames(df_tb) <- c("country","participants")
tribble_cnty <- df_tb %>% group_by(participants)
tribble_cnty <- tribble_cnty %>% summarise(countries_by_participants = n())
tribble_cnty$participants_labels <- as.factor(tribble_cnty$participants)

barplot(tribble_cnty$countries_by_participants,
        names.arg = tribble_cnty$participants,
        xlab="Countries with N participants",
        ylab="Number of countries",
        main="Countries by number of participants - E2",
        ymin=0, ymax=15
        )

ggplot(data=tribble_cnty, aes(x=participants_labels, y=countries_by_participants)) +
  geom_bar(stat="identity", fill="lightgray")+
  geom_text(aes(label=countries_by_participants), vjust=-0.3, color="black", size=3.0)+
  theme_minimal()+
  theme(plot.title = element_text("Helvetica-Narrow", face="plain", colour="black", size=10))+
  theme(axis.title.x = element_text("Helvetica-Narrow", face="plain", colour="black", size=8))+
  theme(axis.title.y = element_text("Helvetica-Narrow", face="plain", colour="black", size=8))+
  theme(axis.text.x  = element_text("Helvetica-Narrow", face="plain", colour="black", size=8))+
  theme(axis.text.y  = element_text("Helvetica-Narrow", face="plain", colour="black", size=8))+
  xlab("Number of participants")+
  ylab("Number of countries")+
  ggtitle("Countries by number of participants - E2")

df_E2$country <- unlist(lapply(df_E2$country, 
                               function(v) {
                                 if(v %in%  c("US","INDIA")) return(v)
                                 else return("OTHER")
                               }))

df_E2$country_labels<- factor(df_E2$country,
                              levels = c("US","INDIA","OTHER")
)

df_E2$country_id<- factor(df_E2$country_labels, 
                          levels=levels(df_E2$country_labels),
                          labels = c(1:3)
)

#DURATION in Minutes
df_E2$testDuration_minutes <- df_E2$testDuration/(1000*60)

df_E2$duration_minutes <- df_E2$duration/(1000*60)


#Transform isBugCovering as factor again
df_E2$isBugCovering <- factor(df_E2$isBugCovering, 
                           levels = c("TRUE","FALSE")
                          )
df_E2$isBugCovering_id <- factor(df_E2$isBugCovering,
                                 levels=levels(df_E2$isBugCovering),
                                 labels=c(1,0)
                          )


#Transform answer as factor again
df_E2$answer <- factor(df_E2$answer, 
                       levels = c("YES_THERE_IS_AN_ISSUE",
                                  "NO_THERE_IS_NOT_AN_ISSUE",
                                  "I_DO_NOT_KNOW")
                       )
df_E2$answer_id <- factor(df_E2$answer,
                          levels=levels(df_E2$answer),
                          labels=c(1,0,2)
                    )
