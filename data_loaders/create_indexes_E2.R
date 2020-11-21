
"
Create indexes and format fields

"

create_indexes <- function(data_E2){
  
  "PROFESSION"
  #Convert profession from factor to character 
  data_E2$profession <- as.character(data_E2$profession)
  #Replaces 'Other...something' for only 'Other'
  data_E2[grep("Other",data_E2$profession),"profession"] <- "Other"
  #Transform profession as factor again
  data_E2$profession <- factor(data_E2$profession, 
                             levels = c("Professional_Developer","Hobbyist",
                                        "Graduate_Student","Undergraduate_Student",
                                        "Other")
  )
  data_E2$profession_id <- factor(data_E2$profession,
                                levels=levels(data_E2$profession),
                                labels=c(1:5)
  )
  
  "YEARS OF PROGRAMMING"
  data_E2$years_programming <- as.numeric(data_E2$years_programming)
  
  "QUALIFICATION_SCORE"
  
  #Transform profession as factor again
  
  data_E2$qualification_score_label<- factor(data_E2$qualification_score, 
                                           levels = c(5:0),
                                           labels = c("100%","80%","60%","40%","20%","0%")
  )
  data_E2$qualification_score_id <- factor(data_E2$qualification_score_label,
                                         levels=levels(data_E2$qualification_score_label),
                                         labels=c(5:0)
  )
  data_E2$qualification_score <- as.numeric(data_E2$qualification_score);
  
  
  "FILE_NAME"
  
  data_E2 <- data_E2[data_E2$file_name!="null",]
  
  data_E2$file_name<- factor(data_E2$file_name, 
                           levels = c("HIT01_8","HIT02_24","HIT03_6","HIT04_7",
                                      "HIT05_35","HIT06_51","HIT07_33","HIT08_54")
  )
  data_E2$file_name_id <- factor(data_E2$file_name,
                               levels=levels(data_E2$file_name),
                               labels=c(1:8)
  )
  
  "GENDER"
  
  data_E2$gender<- factor(data_E2$gender, 
                        levels = c("Female","Male","Prefer_not_to_tell","Other")
  )
  
  data_E2$gender_id<- factor(data_E2$gender, 
                           levels=levels(data_E2$gender),
                           labels = c(1:4)
  )
  
  "AGE"
  
  data_E2$age <- as.numeric(data_E2$age);
  
  
  "COUNTRY"
  
  
  data_E2$country <- unlist(lapply(data_E2$country, function(v) {
    if (is.character(v)) return(toupper(v))
    else return(v)
  }))
  
  data_E2$country <- gsub("\\bUNITED STATES OF AMERICA\\b","US",data_E2$country)
  data_E2$country <- gsub("\\bUNITED STATES\\b","US",data_E2$country)
  data_E2$country <- gsub("\\bUNITES STATES\\b","US",data_E2$country)
  data_E2$country <- gsub("\\bUNITEDE STATES\\b","US",data_E2$country)
  data_E2$country <- gsub("\\bUNITED STAETS\\b","US",data_E2$country)
  data_E2$country <- gsub("\\bUNITED\\b","US",data_E2$country)
  data_E2$country <- gsub("\\bUNIT\\b","US",data_E2$country)
  data_E2$country <- gsub("\\bAMERICA\\b","US",data_E2$country)
  data_E2$country <- gsub("LOS ANGELES","US",data_E2$country)
  data_E2$country <- gsub("ILLINOIS","US",data_E2$country)
  data_E2$country <- gsub("WISCONSIN","US",data_E2$country)
  data_E2$country <- gsub("RIVERSIDE","US",data_E2$country)
  data_E2$country <- gsub("SAN JOSE CA","US",data_E2$country)
  data_E2$country <- gsub("SAN DIEGO","US",data_E2$country)
  data_E2$country <- gsub("LOUISIANA","US",data_E2$country)
  data_E2$country <- gsub("CALIFORNIA","US",data_E2$country)
  data_E2$country <- gsub("ARIZONA","US",data_E2$country)
  data_E2$country <- gsub("OHIO","US",data_E2$country)
  data_E2$country <- gsub("MARYLAND","US",data_E2$country)
  data_E2$country <- gsub("CINCINNATI","US",data_E2$country)
  data_E2$country <- gsub("\\bIL\\b","US",data_E2$country)
  data_E2$country <- gsub("\\bIN\\b","US",data_E2$country)
  
  data_E2$country <- gsub("U\\.S\\.A\\.","US",data_E2$country)
  data_E2$country <- gsub("U\\.S\\.","US",data_E2$country)
  data_E2$country <- gsub("U\\.S","US",data_E2$country)
  data_E2$country <- gsub("\\bUSA\\b","US",data_E2$country)
  data_E2$country <- gsub("GG","OTHER",data_E2$country)
  data_E2$country <- gsub("NO","OTHER",data_E2$country)
  data_E2$country <- gsub("GURGAON, INDIA","INDIA",data_E2$country)
  data_E2$country <- gsub("INDIAN","INDIA",data_E2$country)
  data_E2$country <- gsub("MADURAI","INDIA",data_E2$country)
  data_E2$country <- gsub("SRILANKA","SRI LANKA",data_E2$country)
  data_E2$country <- gsub("ENGLAND","UK",data_E2$country)
  
  df_tb <-  data.frame(table(data_E2$country))
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
  
  data_E2$country <- unlist(lapply(data_E2$country, 
                                 function(v) {
                                   if(v %in%  c("US","INDIA")) return(v)
                                   else return("OTHER")
                                 }))
  
  data_E2$country_labels<- factor(data_E2$country,
                                levels = c("US","INDIA","OTHER")
  )
  
  data_E2$country_id<- factor(data_E2$country_labels, 
                            levels=levels(data_E2$country_labels),
                            labels = c(1:3)
  )
  
  #DURATION in Minutes
  data_E2$testDuration_minutes <- data_E2$testDuration/(1000*60)
  
  data_E2$duration_minutes <- data_E2$duration/(1000*60)
  
  
  #Transform isBugCovering as factor again
  data_E2$isBugCovering <- factor(data_E2$isBugCovering, 
                                levels = c("TRUE","FALSE")
  )
  data_E2$isBugCovering_id <- factor(data_E2$isBugCovering,
                                   levels=levels(data_E2$isBugCovering),
                                   labels=c(1,0)
  )
  
  
  #Transform answer as factor again
  data_E2$answer <- factor(data_E2$answer, 
                         levels = c("YES_THERE_IS_AN_ISSUE",
                                    "NO_THERE_IS_NOT_AN_ISSUE",
                                    "I_DO_NOT_KNOW")
  )
  data_E2$answer_id <- factor(data_E2$answer,
                            levels=levels(data_E2$answer),
                            labels=c(1,0,2)
  )

  data_E2$explanation.size <- as.numeric(sapply(strsplit(as.character(data_E2$explanation)," "), length));
  
  data_E2$isAnswerCorrect <- as.factor(data_E2$isAnswerCorrect)
  
  #Transform answer as factor again
  #data_E2$isAnswerCorrect_bol <- as.integer(as.logical(data_E2$isAnswerCorrect))
  
  # data_E2$isAnswerCorrect <- factor(data_E2$isAnswerCorrect, 
  #                        levels = c("FALSE","TRUE")
  # )
  # data_E2$isAnswerCorrect_id <- factor(data_E2$isAnswerCorrect,
  #                           levels=levels(data_E2$isAnswerCorrect),
  #                           labels=c(0,1)
  # )
  
  print("data loaded on data_E2")
  
  return(data_E2);
}
