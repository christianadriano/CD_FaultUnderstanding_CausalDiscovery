"
-------------------------------------------------------
Loads the ground truth for both experiment E2.
Stores the data as a new column in the data frame 
Colunm name is 'ground_truth'

This script also evaluates each answer against the grond truth.
The result ist stored in a new data frame column named 'answer_correct'
The values of this colums are 'TRUE' or 'FALSE'

For E1, the following comparision give a TRUE value for 'answer_correct':
if ((answer = YES OR PROBABLY_YES) AND ground_truth = YES)
if ((answer = NO or PROBABLY_NOT or IDK) AND grond_truth = NO)

Any other condition will set the column 'answer_correct' to FALSE

------------------------------------------------------
"

library(tidyverse) #includes dplyr, stringr,tidyr,ggplot2,tibble,purr,forcats

path <- "C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data//"

"
Load Ground Truth to E2 data
"
#df_E2 <- readARFF(str_c(path, "//consolidated_Final_Experiment_2.arff"))

#Tasks data with code complexity information
df_tasks <- read.csv(str_c(path, "merged_tasks_complexity_E2.csv"))

#Score factors computed through IRT Model fitting
df_irt <- read.csv("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//E2_QualificationTest_IRT.csv")
df_irt <-  dplyr::select(df_irt, worker_id,z1)


#Ground truth for tasks
df_truth <- read.csv(str_c(path, "ground_truth_E2.csv"))

#Select only the columns that will be left joined with df_E2
df_truth <-  dplyr::select(df_truth, ID,LineID,isBugCovering,type,faulty_lines,all_Lines,LOC_inspection,LOC_original)



"Left-join microtask_id,ID"
df_truth$ID <- as.factor(df_truth$ID) #convert to factor, so I can join with microtask_id column
df_tasks$microtask_id <- as.factor(df_tasks$microtask_id)
df_tasks$worker_id <- as.factor(df_tasks$worker_id)
df_E2_ground <- left_join(df_tasks,df_truth,by=c("microtask_id"="ID"))

"Left-join worker_id, worker_id"
df_irt$worker_id <- as.factor(df_irt$worker_id) #convert to factor, so I can join with microtask_id column
df_E2_ground$worker_id <- as.factor(df_E2_ground$worker_id) #convert to factor, so I can join with microtask_id column
df_E2_ground <- inner_join(df_E2_ground,df_irt,by=c("worker_id"="worker_id"))
#dim(df_E2_ground) [1] 1462   54 Should have 2580 rows!

"
Apply Ground Truth to E2 data
"
isCorrectList <- (df_E2_ground$answer=="YES_THERE_IS_AN_ISSUE" &  df_E2_ground$isBugCovering) |
  (
    (df_E2_ground$answer=="NO_THERE_IS_NOT_AN_ISSUE" | df_E2_ground$answer=="I_DO_NOT_KNOW") & 
      !df_E2_ground$isBugCovering
  )


df_E2_ground$isAnswerCorrect <- isCorrectList


