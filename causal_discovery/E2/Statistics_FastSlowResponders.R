"
Fast & Slow Responders
Comparative Statistics and Distributions Plots for 
- test_duration
- progr_years
- age_years
"

library(ggplot2)
library(moments)

prepareData <- function(){
  #Load only Consent data. No data from tasks, only from demographics and qualification test
  source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E2.R")
  df_consent <- load_consent_create_indexes()
  df_consent <- rename(df_consent,progr_years=years_programming)
  df_consent <- rename(df_consent,test_score=adjusted_score)
  df_consent <- rename(df_consent,age_years=age)
  df_consent <- rename(df_consent,fast_classif=testDuration_fastMembership)
  return(df_consent)
}

df_consent <- prepareData()


df_selected <-
  dplyr::select(df_consent,
                profession,
                fast_classif,
                progr_years,
                age_years,
                test_duration,
                test_score #outcome variable
  );

