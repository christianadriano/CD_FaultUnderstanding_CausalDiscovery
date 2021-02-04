"
Save CONSENT data from E2 to CSV file

The CONSENT data was processes by file load_consent_create_indexes_E2.R

Data sources that composed the final csv file are:
- consent_consolidated_Experiment_2.arff (produced by project DW_Microtasks)
- E2_QualificationTest_IRT.csv (produced by scripts in CausalModel_FaultUnderstanding item_response_model)

"

source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E2.R")

write.csv(df_consent,
          "C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data//Indexed_Consent_E2.csv",
          row.names = FALSE
          )

#---------------------
#END
#---------------------

