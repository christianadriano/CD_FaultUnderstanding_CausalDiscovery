#STUDY THE collinearity of Yoe and Age

#Load data
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E2.R")

df_consent <- load_consent_create_indexes()
#Remove NA's
df_E2 <- df_consent[complete.cases(df_consent[,c("years_programming","qualification_score")]),]

# standardize variables = (zero centered, standard deviation one)
df_E2$yoe <- scale(df_E2$years_programming)
df_E2$score <- scale(df_E2$qualification_score)
df_E2$age <- scale(df_E2$age)

#------------------
#ALL

cor.test(x=df_E2$yoe, y=df_E2$age, method="kendall")
#z = 15.679, p-value < 2.2e-16, tau=0.2647107 
#Medium correlated.

#------------------------------
#By Profession


