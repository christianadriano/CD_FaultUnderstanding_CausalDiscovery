" 
Does Yoe and Age affect duration of tests?
"

library(rethinking)
library(stringr)
library(dplyr)

#Load data
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//load_create_indexes_E2.R")

df_E2$yoe <- scale(df_E2$years_programming)
df_E2$ages <- scale(df_E2$age)

#-----------------
"OUTLIERS in DURATION"
"Code comprehension studies show that a programmer takes from 12 to 24 seconds is also the 
average minimum time to read one line of code."

"The lower cut to the minimum time to read all 5 questions and corresponding lines of code
in the qualification test. 
 Since the test has 5 questions, each question 
requires the inspection of one line of code, that would require the programmer from 60s to 120s.
We chose 60s (1 min) as the minimum time-effort one need to read and answer all 5 questions"

#The upper cut corresponds to 3 times the minimum. We choose 24s, so 60 min.

df_E2_aux <- df_E2[df_E2$testDuration_minutes<=12 & df_E2$testDuration_minutes>=1 ,]
boxplot(df_E2_aux$testDuration_minutes)
summary(df_E2_aux$testDuration_minutes)