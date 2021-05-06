"
Merge tasks data with consent data for E2

Files involved

load_consent_create_indexes_E2.R
load_ground_truth_E2.R
create_indexes_E2.R

- Loads merged data of Tasks and Consent (demographics) from file merged_tasks_complexity_E2.csv
- Loads the ground truth for experiment E2

the consent data already includes 
- Includes IRT (item response theory) equalization of the grades

- Includes Add Halstead complexity of each program statement

TODO:
(DONE) Test if the data is correct. The last step shrunk from 85 to 66 columns. Strange.
(DONE) Drop duplicate columns
(DONE) Remove suffixes of the remaining ones

(DONE) Check if qualification score and profession matches for same worker id

  Check file matching_errors.csv 
  1- There seems to be problem in the generaion of IRT file, 
  which is being later merged by the script load_consent_create_indexes()?
  - I think the problem is that some workers took the tests more than once and I kept their original scores.
  - Moreover, while running the IRT, I took only one of these scores.
  SOlution is to average these multiple scores and then, run the IRT adjustment.
  
  2- If there are different demographics w.r.t. profession, years of experience, and age, I need to pick one.
 
  
"

#----------------
#Load tasks with ground truth and complexity
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_ground_truth_E2.R")
df_E2_ground <- load_ground_truth();

#TODO Filter out fields in the ground truth that already exist in df_consent_selected


#----------------
#Load consent data
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E2.R")
df_consent <- load_consent_create_indexes();

#select only the columns that will be necessary to add to the tasks data
df_consent_selected <- select(df_consent,worker_id,file_name,adjusted_score, qualification_score,testDuration_fastMembership, is_fast)

#filter out workers who did not qualify for the test, because there is no task data for them
df_consent_selected <- df_consent_selected[df_consent_selected$qualification_score>=3,]

#----------------
#Merge worker and task data

"Left-join worker_id, worker_id"
df_E2_final <- left_join(x=df_E2_ground,y=df_consent_selected,keep=FALSE, by=c("worker_id"="worker_id"))
dim(df_E2_final) 

#Testing the results, qualification_score is not matching.
df_E2_final$qualification_score.x==df_E2_final$qualification_score.y


#----------------
#Create indexes
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//create_indexes_E2.R")
df_E2_indexed <- create_indexes(df_E2_final)
dim(df_E2_indexed)
