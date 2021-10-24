
"
CONSENT Data E2

Comparative Study of the Outcomes of Traditional Outlier Detection Methods

Goal(Goal-Base Question Metric):

What: Compare the outcomes of different outlier detection methods
Why-1: mitigate wrong outlier detection might lead incorrect understanding of the data (construct validity threat)
why-2: increase the reproducibility of the data wrangling procedure
How: Traditional outlier detection methods (Boxplot, MAD, Adjusted Boxplot)
Data: Crowdsourced answers from programmers about location of software faults


Gist of results:
Method M1 produced was prefered for dataset D1 because .....

TODO: 
- Should outlier of TEST DURATION be adjusted by the number of correct answers? Because people who did not read the answer, might have gotten it wrong.
Maybe it does not make sense to have an automatic outlier detection for test duration...
- Revise the Grubber method and see if there is some discussion about it.

"

#install.packyears_programmings("outliers")
library(outliers)
library(farff)
library(tidyr)

#Load data
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E2.R")
df_consent <- load_consent_create_indexes()
dim(df_consent) #1788

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



