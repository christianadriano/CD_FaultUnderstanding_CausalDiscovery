#STUDY THE collinearity of Yoe and Age

#Load data
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E2.R")

df_consent <- load_consent_create_indexes()
#Remove NA's
df_E2 <- df_consent[complete.cases(df_consent[,c("years_programming","age")]),]

# standardize variables = (zero centered, standard deviation one)
df_E2$yoe <- scale(df_E2$years_programming)
df_E2$age <- scale(df_E2$age)

#------------------
#ALL

cor.test(x=df_E2$yoe, y=df_E2$age, method="kendall")
#z = 15.679, p-value < 2.2e-16, tau=0.2647107 
#Medium correlated.

#------------------------------
#By Profession

profession_list <- unique(df_E2$profession)

for (prof in profession_list) {
  df_prof <- df_E2[df_E2$profession==prof,]
  r <- cor.test(x=df_prof$yoe, y=df_prof$age, method="kendall")
  print(paste0(prof,": statistic = ",round(r$statistic,4),
              ", p-value = ",round(r$p.value,4),
              ", tau = ",round(r$estimate,4)
              )
        )
}

#Non-Significant
 # "Undergraduate_Student: statistic = -0.2207, p-value = 0.8253, tau = -0.0078"
 # "Other: statistic = 0.5907, p-value = 0.5547, tau = 0.0413"
 # "Graduate_Student: statistic = 1.772, p-value = 0.0764, tau = 0.0785"

#Significant and Medium or Strong
 # "Hobbyist: statistic = 7.524, p-value = 0, tau = 0.2458"
 # "Professional: statistic = 14.8838, p-value = 0, tau = 0.5173"
 # "Programmer: statistic = 3.0979, p-value = 0.0019, tau = 0.3193"


