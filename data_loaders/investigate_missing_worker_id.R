"
Investigate worker-ids in Tasks that are not in Consent
"

library(tidyverse) #includes dplyr, stringr,tidyr,ggplot2,tibble,purr,forcats

path <- "C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data//"
df_tasks <- read.csv(str_c(path, "merged_tasks_complexity_E2.csv"))

#Score factors computed through IRT Model fitting
df_irt <- read.csv("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data//irt//E2_QualificationTest_IRT.csv")
df_irt <-  dplyr::select(df_irt, worker_id,z1) #file_name,z1) #need file_name, because a few workers have more than one score.

"Left-join worker_id, worker_id"
df_irt$worker_id <- as.factor(df_irt$worker_id) #convert to factor, so I can join with microtask_id column
df_tasks$worker_id <- as.factor(df_tasks$worker_id) #convert to factor, so I can join with microtask_id column
#only joins with people who qualified (qualification_score>=3), because only these are present in the task execution logs


df_all_in_task <- left_join(x=df_tasks,y=df_irt,keep=TRUE, by=c("worker_id"="worker_id"))
dim(df_all_in_task) 

df_in_both <- inner_join(x=df_tasks,y=df_irt,keep=TRUE, by=c("worker_id"="worker_id"))
dim(df_in_both)


