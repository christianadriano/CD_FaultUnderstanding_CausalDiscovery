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

df_E2 <- read.csv(str_c(path, "merged_tasks_complexity_E2.csv"))

gt2 <- read.csv(str_c(path, "ground_truth_E2.csv"))

#Select only the columns that will be left joined with df_E2
gt2 <-  select(gt2, ID,LineID,isBugCovering,type,faulty_lines,all_Lines,LOC_inspection,LOC_original)



"Lef-join microtask_id,ID"
gt2$ID <- as.factor(gt2$ID) #convert to factor, so I can join with microtask_id column
df_E2$microtask_id <- as.factor(df_E2$microtask_id)
df_E2_ground <- left_join(df_E2,gt2,by=c("microtask_id"="ID"))


"
Apply Ground Truth to E2 data
"
isCorrectList <- (df_E2_ground$answer=="YES_THERE_IS_AN_ISSUE" &  df_E2_ground$isBugCovering) |
  (
    (df_E2_ground$answer=="NO_THERE_IS_NOT_AN_ISSUE" | df_E2_ground$answer=="I_DO_NOT_KNOW") & 
     df_E2_ground$isBugCovering =="no"
   )


df_E2_ground$isAnswerCorrect <- isCorrectList


