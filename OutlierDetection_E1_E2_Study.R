
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

#install.packages("robustbase")
#library(robustbase)
install.packages("outliers")
library(outliers)
library(farff)

path <- "C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data//"
dataset_E2 <- readARFF(paste0(path, "consent_consolidated_Experiment_2.arff"))
df_E2 <- data.frame(dataset_E2)
df_consent <- data.frame(dataset_E2)
dim(df_consent) #3658

#Processing Age, so remove rows where Age==NA
df_consent$experience <- as.factor(df_consent$experience)
df_consent_age <- df_consent[!is.na(df_consent$age),]
dim(df_consent_age)  #2463

#GRUBBS TEST - https://www.statsandr.com/blog/outliers-detection-in-r/
outliers::grubbs.test(df_consent_age[df_consent_age$experience=="Undergraduate_Student",]$age)
#100 is an outlier

#substitute the outlier for the median value
df_consent_age[df_consent$experience=="Undergraduate_Student" &
             df_consent_age$age==100,]$age <- median(df_consent_age[
                                                  df_consent_age$experience=="Undergraduate_Student","age"])


outliers::grubbs.test(df_consent[df_consent$experience=="Professional_Developer",]$age)
#57 is an outlier

outliers::grubbs.test(df_consent[df_consent$experience=="Graduate_Student",]$age)
#57 is an outlier

outliers::grubbs.test(df_consent[df_consent$experience=="Hobbyist",]$age)
#66 is an outlier


