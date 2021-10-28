
"
CONSENT Data E2

Comparative Study of the Outcomes of Traditional Outlier Detection Methods


There are various outliers tests, the following three Grubbs test [Grubbs 1969], 
Tietjen-Moore test (Tietjen & Moore 1972)and the extreme Studentized deviate) ESD test (Rosner 1983). 

The Grubbs test is to verify the presence of a single outlier, while the Tiejen-Moore and the ESD tests are for multiple outliers. 

However, the Tiejen-Moore requires that the number of outliers be given as input for the test).
All these tests assume that the distribution is univariate and normal (i.e., Gaussian).

Grubbs, Frank (February 1969), Procedures for Detecting Outlying Observations in Samples, Technometrics, 11(1), pp. 1-21.

Rosner, Bernard (May 1983), Percentage Points for a Generalized ESD Many-Outlier Procedure,Technometrics, 25(2), pp. 165-172.

Tietjen and Moore (August 1972), Some Grubbs-Type Statistics for the Detection of Outliers, Technometrics, 14(3), pp. 583-597.

Goal(Goal-Base Question Metric):

What: Compare the outcomes of different outlier detection methods
Why-1: mitigate wrong outlier detection might lead incorrect understanding of the data (construct validity threat)
why-2: increase the reproducibility of the data wrangling procedure
How: Traditional outlier detection methods (Boxplot, MAD, Adjusted Boxplot)
Data: Crowdsourced answers from programmers about location of software faults


Gist of results:
None of the distributions are normal, so they do not satisfy the assumptions of the outlier tests.
"

#install.package("outliers")
library(outliers)
library(farff)
library(tidyr)

#Load data
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E2.R")
df_consent <- load_consent_create_indexes()
dim(df_consent) #1788



"Testing for normality"

#AGE
shapiro.test(df_consent$age)
#data:  df_consent$age W = 0.88595, p-value < 2.2e-16 NOT NORMAL

shapiro.test(df_consent$years_programming)
#data:  df_consent$age W = 0.71584, p-value < 2.2e-16 NOT NORMAL

"None of the distributions are normal, so they do not satisfy the assumptions of the outlier tests."


"Below is only a study using the Grubbs test"

#---------------------------------------
# AGE



#GRUBBS TEST - https://www.statsandr.com/blog/outliers-detection-in-r/
outliers::grubbs.test(df_consent[df_consent$profession=="Undergraduate_Student",]$age,opposite = FALSE)
#highest value 61 is an outlier
outliers::grubbs.test(df_consent[df_consent$profession=="Undergraduate_Student",]$age,opposite = TRUE)
#lowest value 18 is an outlier

#substitute the outlier for the median value
df_consent[df_consent$profession=="Undergraduate_Student" &
             df_consent$age==61,]$age <- median(df_consent[
                                                  df_consent$profession=="Undergraduate_Student",]$age)


outliers::grubbs.test(df_consent[df_consent$profession=="Professional",]$age, opposite = FALSE)
#highest value 57 is an outlier
outliers::grubbs.test(df_consent[df_consent$profession=="Professional",]$age, opposite = TRUE)
#lowest value 21 is an outlier

#substitute the outlier for the median value
df_consent[df_consent$profession=="Professional" &
                 df_consent$age==21,]$age <- median(df_consent[
                   df_consent$profession=="Professional",]$age)


outliers::grubbs.test(df_consent[df_consent$profession=="Graduate_Student",]$age, opposite = FALSE)
#highest value 57 is an outlier
outliers::grubbs.test(df_consent[df_consent$profession=="Graduate_Student",]$age, opposite = TRUE)
#lowest value 21 is an outlier
df_consent[df_consent$profession=="Graduate_Student" & df_consent$age==21,]$age <- median(df_consent[df_consent$profession=="Graduate_Student",]$age)

outliers::grubbs.test(df_consent[df_consent$profession=="Hobbyist",]$age, opposite=FALSE)
#highest value 66 is an outlier
outliers::grubbs.test(df_consent[df_consent$profession=="Hobbyist",]$age, opposite=TRUE)
#lowest value 18 is an outlier

#-----------------------------------------------------------------------------

# Years of Programming (experience)

#GRUBBS TEST - https://www.statsandr.com/blog/outliers-detection-in-r/
outliers::grubbs.test(df_consent[df_consent$profession=="Undergraduate_Student",]$years_programming,opposite = FALSE)
#highest value 20 is an outlier
outliers::grubbs.test(df_consent[df_consent$profession=="Undergraduate_Student",]$years_programming,opposite = TRUE)
#lowest value 0 is an outlier

#substitute the outlier for the median value
df_consent[df_consent$profession=="Undergraduate_Student" &
             df_consent$years_programming==20,]$years_programming <- median(df_consent[
               df_consent$profession=="Undergraduate_Student",]$years_programming)


outliers::grubbs.test(df_consent[df_consent$profession=="Professional",]$years_programming, opposite = FALSE)
#highest value 35 is an outlier
outliers::grubbs.test(df_consent[df_consent$profession=="Professional",]$years_programming, opposite = TRUE)
#lowest value 0 is an outlier

#substitute the outlier for the median value
df_consent[df_consent$profession=="Professional" &
             df_consent$years_programming==35,]$years_programming <- median(df_consent[
               df_consent$profession=="Professional",]$years_programming)


outliers::grubbs.test(df_consent[df_consent$profession=="Graduate_Student",]$years_programming, opposite = FALSE)
#highest value 17 is an outlier
outliers::grubbs.test(df_consent[df_consent$profession=="Graduate_Student",]$years_programming, opposite = TRUE)
#lowest value 0 is an outlier
df_consent[df_consent$profession=="Graduate_Student" & df_consent$years_programming==17,]$years_programming <- median(df_consent[df_consent$profession=="Graduate_Student",]$years_programming)

outliers::grubbs.test(df_consent[df_consent$profession=="Hobbyist",]$years_programming, opposite=FALSE)
#highest value 35 is an outlier
outliers::grubbs.test(df_consent[df_consent$profession=="Hobbyist",]$years_programming, opposite=TRUE)
#lowest value 0 is an outlier

#-----------------------------------------------------------------------------

# TEST DURATION 
outliers::grubbs.test(df_consent[df_consent$profession=="Undergraduate_Student",]$test_duration,opposite = FALSE)
# highest value 14.7441 is an outlier
outliers::grubbs.test(df_consent[df_consent$profession=="Undergraduate_Student",]$test_duration,opposite = TRUE)
# lowest value 0.399733333333333 is an outlier

outliers::grubbs.test(df_consent[df_consent$profession=="Graduate_Student",]$test_duration,opposite = FALSE)
# highest value 14.60075 is an outlier
outliers::grubbs.test(df_consent[df_consent$profession=="Graduate_Student",]$test_duration,opposite = TRUE)
# lowest value 0.346966666666667 is an outlier

outliers::grubbs.test(df_consent[df_consent$profession=="Hobbyist",]$test_duration,opposite = FALSE)
# highest value 15.3639333333333 is an outlier
outliers::grubbs.test(df_consent[df_consent$profession=="Hobbyist",]$test_duration,opposite = TRUE)
# lowest value 0.57015 is an outlier



