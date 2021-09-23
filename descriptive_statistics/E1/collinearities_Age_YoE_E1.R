#E1  STUDY THE collinearity of Yoe and Age

#Load data
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E1.R")

df_consent <- load_consent_create_indexes(load_is_student=1)
#Remove NA's
df_E1 <- df_consent[complete.cases(df_consent[,c("years_programming","age")]),]

# standardize variables = (zero centered, standard deviation one)
df_E1$yoe <- scale(df_E1$years_programming)
df_E1$age <- scale(df_E1$age)

#------------------
#ALL

cor.test(x=df_E1$yoe, y=df_E1$age, method="kendall")
#z = 3.9701, p-value = 7.186e-05, tau=0.1301779 
#Weak correlation 

#------------------------------
#By Profession

profession_list <- unique(df_E1$profession)

for (prof in profession_list) {
  df_prof <- df_E1[df_E1$profession==prof,]
  r <- cor.test(x=df_prof$yoe, y=df_prof$age, method="kendall")
  print(paste0(prof,": statistic = ",round(r$statistic,4),
              ", p-value = ",round(r$p.value,4),
              ", tau = ",round(r$estimate,4)
              )
        )
}

# Non-Significant
#  "non-student: statistic = 3.4491, p-value = 6e-04, tau = 0.1238"
#  "student: statistic = 1.2751, p-value = 0.2023, tau = 0.1152"

