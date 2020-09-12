"
Propensity score matching
"

install.packages("tableone")
install.packages("Matching")
install.packages("MatchIt")

library(tableone)
library(Matching)
#Now load the lalonde data (which is in the MatchIt package):
library(MatchIt)
library(dplyr)

data(lalonde)
#df <- scale(lalonde[c("age", "educ", "re74", "re75"),])

df <- lalonde %>% mutate_at(
                      c("age", "educ","married", "re74", "re75","re78"), 
                      ~(scale(.) %>% as.vector)
                      ) 

#For each variable,find the standardized differences (mean treatment - mean control)
#Age
df_married <- df[df$married==1,]
mean_Age_diff <- mean(df_married[df_married$treat==1,]$age) -  mean(df_married[df_married$treat==0,]$age)
mean_Edu_diff <-  mean(df_married[df_married$treat==1,]$edu) -  mean(df_married[df_married$treat==0,]$edu)
mean_re74_diff <-  mean(df_married[df_married$treat==1,]$re74) -  mean(df_married[df_married$treat==0,]$re74)
mean_re75_diff <-  mean(df_married[df_married$treat==1,]$re75) -  mean(df_married[df_married$treat==0,]$re75)

mean_Age_diff + mean_Edu_dif + mean_re74_diff + mean_Edu_re75

married_1 = df[df$treat==1,]$married
married_0 = df[df$treat==0,]$married

avg_married_1 = mean(married_1)
avg_married_0 = mean(married_0)
var_married_1 = var(married_1)
var_married_0 = var(married_0)

#difference between the means divided by the square-root of the sum
#sample variances divided by 2
(avg_married_0 - avg_married_1) / (sqrt((var_married_0+var_married_1)/2))
#[1] 0.719492


mean(lalonde[lalonde$treat==1,]$re78) - mean(lalonde[lalonde$treat==0,]$re78)
mean(df$married)
