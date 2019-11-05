"
Load data from E2 and create indexes for: profession, qualification_score, file_name,
answer.

"

path <- "C://Users//Christian//Documents//GitHub//Complexity_Metrics//output//"
dataset_E2 <- read.csv(str_c(path, "merged_tasks_complexity_E2.csv"))
df_E2 <- data.frame(dataset_E2)

"PROFESSION"
#Convert profession from factor to character 
df_E2$profession <- as.character(df_E2$profession)
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

"ANSWER"

df_E2$answer<- factor(df_E2$answer, 
                         levels = c("I_DO_NOT_KNOW",
                                    "NO_THERE_IS_NOT_AN_ISSUE",
                                    "YES_THERE_IS_AN_ISSUE")
)
df_E2$answer_id <- factor(df_E2$answer,
                             levels=levels(df_E2$answer),
                             labels=c(1:3)
)

"Buggy not Buggy"
