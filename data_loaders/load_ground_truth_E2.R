"-------------------------------------------------------
Load and merge the ground truth data of the tasks and also the complexity of tasks (LOC, Halstead)

Regarding the ground truth data:
Stores the data as a new colunm name is 'ground_truth'

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

load_ground_truth <- function(){
  
  path <- "C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data//"
  
  
  #Tasks data with code complexity information
  df_tasks <- read.csv(str_c(path, "merged_tasks_complexity_E2.csv"))
  
  #Ground truth for tasks
  df_truth <- read.csv(str_c(path, "ground_truth_E2.csv"))
  
  #Select only the columns that will be left joined with df_E2
  df_truth <-  dplyr::select(df_truth, ID,LineID,isBugCovering,type,faulty_lines,all_Lines,LOC_inspection,LOC_original)
  
  "Left-join microtask_id,ID"
  df_truth$ID <- as.factor(df_truth$ID) #convert to factor, so I can join with microtask_id column
  df_tasks$microtask_id <- as.factor(df_tasks$microtask_id)
  df_E2_ground <- left_join(df_tasks,df_truth,by=c("microtask_id"="ID"))
  
  
  #Apply Ground Truth to E2 data
  isCorrectList <- (df_E2_ground$answer=="YES_THERE_IS_AN_ISSUE" &  df_E2_ground$isBugCovering) |
    (
      (df_E2_ground$answer=="NO_THERE_IS_NOT_AN_ISSUE" | df_E2_ground$answer=="I_DO_NOT_KNOW") & 
        !df_E2_ground$isBugCovering
    )
  df_E2_ground$isAnswerCorrect <- isCorrectList
  
  print(paste0("Loaded ",dim(df_E2_ground)[1], " rows."," Results are in df_E2_ground"))
  return(df_E2_ground)
}