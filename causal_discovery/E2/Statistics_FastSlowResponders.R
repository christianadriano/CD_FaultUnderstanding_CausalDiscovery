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

professions <- unique(df_consent$profession)

df_selected <-
  dplyr::select(df_consent,
                profession,
                fast_classif,
                progr_years,
                age_years,
                test_duration,
                test_score #outcome variable
  );

#----------------------------------------------------------------------------
#Test DURATION Statistics between Fast and Slow respondents across professions
#----------------------------------------------------------------------------

df_prof <- df_selected[df_selected$profession==choice,]
median_membership_prof <- median(df_prof$testDuration_fastMembership);
df_prof_fast <- df_prof[df_prof$testDuration_fastMembership>=median_membership_prof,]
df_prof_slow <- df_prof[df_prof$testDuration_fastMembership<median_membership_prof,]

boxplot(df_prof_fast$test_duration,main=paste(choice,"Fast","Test Duration"))
boxplot(df_prof_slow$test_duration,main=paste(choice,"Slow","Test Duration"))

