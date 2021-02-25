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
- Drop duplicate columns
- Remove suffixes of the remaining ones

"

#----------------
#Load tasks with ground truth and complexity
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_ground_truth_E2.R")
df_E2_ground <- load_ground_truth();

#----------------
#Load consent data
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E2.R")
df_consent <- load_consent_create_indexes();

#---------------
#Create indexes
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//create_indexes_E2.R")
df_E2_indexed <- create_indexes(df_E2_ground)
dim(df_E2_indexed)

#----------------
#Merge worker and task data

"Left-join worker_id, worker_id"
df_E2_final <- left_join(x=df_E2_indexed,y=df_consent,keep=FALSE, by=c("worker_id"="worker_id","file_name"="file_name"))
dim(df_E2_final) 

#----------------

