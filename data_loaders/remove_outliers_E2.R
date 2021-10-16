
"
E2 Outlier Removals

This script is responsible to centralize the application of the rules for 
test duration, task duration, and explanation.

TODO:
- Outlier Task duration
- Outlier Task explanation size
- Convert to RMD file

"


"
OUTLIERS in TEST DURATION
Code comprehension studies show that a programmer takes from 12 to 24 seconds is also the 
average minimum time to read one line of code.

Because the test has 5 questions, each question requires the inspection of one line of code, 
that would require the programmer from 60s to 120s. 

Lower bound (minimum time) 1 min to read and answer all 5 questions to read all 5 questions and corresponding lines of code

Upper bound (maximum time) is 3 minutes per question, so 15 minutes to read all 5 questions and corresponding lines of code
"
remove_outliers_test_duration <- function(df_dataframe){
  df_dataframe <- df_dataframe[(df_dataframe$test_duration<=15 & df_dataframe$test_duration>=1) ,]
  return(df_dataframe)
}