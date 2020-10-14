"
Relabel Others who are software developers

Keywords: IT, developer, programmer, computer, Tech, Technician, software, computer, QA, DBA, Data, 

"

library(farff)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)

path <- "C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data//"
#path <- "C://Users//Christian//Documents//GitHub//DW_Microtasks//output//"
dataset_E2 <- readARFF(paste0(path, "consent_consolidated_Experiment_2.arff"))
df_E2 <- data.frame(dataset_E2)
df_consent <- data.frame(dataset_E2)
dim(df_consent) #3658

#remove rows without profession information
df_consent <- df_consent[!is.na(df_consent$experience),] #left with 2463

#change to professional so we do no mix up with other developers in the Others category
df_consent[df_consent$experience=="Professional_Developer","experience"] <- "Professional"


#remove rows without test data
dim(df_consent[is.na(df_consent$test1),]) #675 are NA
df_consent <- df_consent[!is.na(df_consent$test1),]
dim(df_consent) #1788 are not NA.

df_others <- df_consent[grep("other",tolower(df_consent$experience)),]
dim(df_others) #161 entries with Other profession who also has test information

pattern <- "it|developer|programmer|computer|tech|technician|software|computer|qa|dba|data"

#create three groups. All other, other developer, other not_developer,
df_consent[(grep(pattern,tolower(df_consent$experience))),"experience"] <- "Programmer"
df_consent[(grep("other",tolower(df_consent$experience))),"experience"] <- "Other"


#Mean qualification score of Other programmers
hist(df_consent[df_consent$experience=="Programmer","qualification_score"])
hist(df_consent[df_consent$experience=="Other","qualification_score"])

t.test(
      df_consent[df_consent$experience=="Programmer","qualification_score"],
      df_consent[df_consent$experience=="Other","qualification_score"],
      alternative = c("two.sided")
)
# t = 6.5172, df = 219.06, p-value = 4.856e-10
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.7241891 1.3520609
# sample estimates:
#   mean of x mean of y 
# 2.960000  1.921875 

t.test(
  df_consent[df_consent$experience=="Programmer","qualification_score"],
  df_consent[df_consent$experience=="Professional","qualification_score"],
  alternative = c("two.sided")
)
# t = -0.91952, df = 40.459, p-value = 0.3633
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.9155798  0.3428380
# sample estimates:
#   mean of x mean of y 
# 2.694444  2.980815

df_consent[(grep(pattern,tolower(df_consent$experience))),"experience"] <- "Professional_Developer"
