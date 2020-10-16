
"
Comparative Study of the Outcomes of Traditional Outlier Detection Methods

Goal(Goal-Base Question Metric):

What: Compare the outcomes of different outlier detection methods
Why-1: mitigate wrong outlier detection might lead incorrect understanding of the data (construct validity threat)
why-2: increase the reproducibility of the data wrangling procedure
How: Traditional outlier detection methods (Boxplot, MAD, Adjusted Boxplot)
Data: Crowdsourced answers from programmers about location of software faults


Gist of results:
Method M1 produced was prefered for dataset D1 because .....

"

#install.packages("outliers")
library(outliers)
library(farff)
library(tidyr)


path <- "C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data//"
dataset_E2 <- readARFF(paste0(path, "consent_consolidated_Experiment_2.arff"))
df_E2 <- data.frame(dataset_E2)
df_consent <- data.frame(dataset_E2)
dim(df_consent) #3658

#rename column from experience to profession
df_consent <- as_tibble(df_consent)
df_consent <- rename(df_consent,profession=experience)

#Processing Age, so remove rows where Age==NA
df_consent$profession <- as.factor(df_consent$profession)
df_consent_age <- df_consent[!is.na(df_consent$age),]
dim(df_consent_age)  #2463

#GRUBBS TEST - https://www.statsandr.com/blog/outliers-detection-in-r/
outliers::grubbs.test(df_consent_age[df_consent_age$profession=="Undergraduate_Student",]$age,opposite = TRUE)
#100 is an outlier
#18 is an outler

#substitute the outlier for the median value
df_consent_age[df_consent_age$profession=="Undergraduate_Student" &
             df_consent_age$age==100,]$age <- median(df_consent_age[
                                                  df_consent_age$profession=="Undergraduate_Student","age"])


outliers::grubbs.test(df_consent_age[df_consent_age$profession=="Professional_Developer",]$age, opposite = TRUE)
#57 is an outlier
#16 is an outlier

#substitute the outlier for the median value
df_consent_age[df_consent_age$profession=="Professional_Developer" &
                 df_consent_age$age==16,]$age <- median(df_consent_age[
                   df_consent_age$profession=="Professional_Developer",]$age)


outliers::grubbs.test(df_consent_age[df_consent_age$profession=="Graduate_Student",]$age, opposite = TRUE)
#57 is an outlier
#2 is an outlier
df_consent_age[df_consent_age$profession=="Graduate_Student" &
                 df_consent_age$age==2,]$age <- median(df_consent_age[
                   df_consent_age$profession=="Graduate_Student",]$age)


outliers::grubbs.test(df_consent[df_consent$profession=="Hobbyist",]$age, opposite=TRUE)
#66 is an outlier
#18 is an outler


