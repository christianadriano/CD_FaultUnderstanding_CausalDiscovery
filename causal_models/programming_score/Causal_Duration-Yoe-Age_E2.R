" 
Does Yoe and Age affect duration of tests?
"

library(rethinking)
library(stringr)
library(dplyr)

#Load data
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//load_consent_create_indexes_E2.R")

dim(df_E2) #3657   32

#Remove participants for which we do not have years_programming
df <- df_E2 %>% drop_na(years_programming) #initial 3567, left with 2062 rows

df$yoe <- scale(df$years_programming)
df$ages <- scale(df$age)

dim(df) #2062 32 Reduced 1595 rows

#-----------------
"OUTLIERS in DURATION"
"Code comprehension studies show that a programmer takes from 12 to 24 seconds is also the 
average minimum time to read one line of code."

"The lower cut to the minimum time to read all 5 questions and corresponding lines of code
in the qualification test. 
 Since the test has 5 questions, each question 
requires the inspection of one line of code, that would require the programmer from 60s to 120s.
We chose 120s (2 min) as the minimum time-effort one need to read and answer all 5 questions"

#The upper cut corresponds to 10 times the minimum. We choose 24s, so 60 min.

df <- df[df$testDuration_minutes<=20 & df$testDuration_minutes>=2 ,]
dim(df_E2) #1732   32

#Remove participants for whom we do not have test duration
df <- df %>% drop_na(testDuration_minutes)
dim(df) #1108   32

boxplot(df$testDuration_minutes)
summary(df$testDuration_minutes)
