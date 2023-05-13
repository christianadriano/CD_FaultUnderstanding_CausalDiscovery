"
Compare Covariate Distributions E1 E2

Covariates:
profession [exogenous];
years_programming [exogenous];
age [exogenous]
test_duration [endogenous];
qualification_score [outcome];
adjusted_score [outcome]; 

"

#Load only Consent data. No data from tasks, only from demographics and qualification test
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E1.R")

df_consent_E1 <- load_consent_create_indexes_E1()

#Load only Consent data. No data from tasks, only from demographics and qualification test
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E2.R")

df_consent_E2 <- load_consent_create_indexes_E2()

#----------------------------------
#Select the data

df_selected_E1 <-
  dplyr::select(df_consent_E1,
                profession, #categorical
                age,
                years_prog,
                test_duration,
                is_fast,
                adjusted_score #outcome
  );


df_selected_E2 <-
  dplyr::select(df_consent_E2,
                profession, #categorical
                age,
                years_prog,
                test_duration,
                is_fast,
                adjusted_score #outcome
  );

#Compare years of programming

ks.test(df_consent_E1$years_prog,df_consent_E2$years_prog)
#D = 0.078894, p-value = 0.01759

ks.test(df_consent_E1[df_consent_E1$is_student==1,]$years_prog,
        df_consent_E2[df_consent_E2$is_student==1,]$years_prog)
#D = 0.060803, p-value = 0.9591 
# Cannot Confirm that Students are statistically significant distinct
# w.r.t. years_prog

ks.test(df_consent_E1[df_consent_E1$is_student==0,]$years_prog,
        df_consent_E2[df_consent_E2$is_student==0,]$years_prog)
#D = 0.16353, p-value = 3.013e-07

