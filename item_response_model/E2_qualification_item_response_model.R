"
Item response model of the programming test qualification E2
"
library(dplyr)
library(ltm)
library(psych)
library(mirt)

source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//load_consent_create_indexes_E2.R")

"Remove participants for whom we did not take the qualification test" 
df_E2 <- df_consent[complete.cases(df_consent[,"qualification_score"]),]
#Original size: 3657   30 , new size 1438 30

"Replace false for 0(zero) and true for one(1)"
# df_E2$test1_ <-  ifelse(df_E2$test1=="true",1,0)
# df_E2$test2_ <-  ifelse(df_E2$test2=="true",1,0)
# df_E2$test3_ <-  ifelse(df_E2$test3=="true",1,0)
# df_E2$test4_ <-  ifelse(df_E2$test4=="true",1,0)
# df_E2$test5_ <-  ifelse(df_E2$test5=="true",1,0)

df <- df_E2 %>% dplyr::select(test1,test2,test3,test4,test5)

write.csv(df,"C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data//irt//E2_QualificationTestResults.csv")


IRT_model <- ltm(df ~ z1, IRT.param=TRUE)

IRT_model
# Coefficients:
#         Dffclt  Dscrmn
# test1   0.018   3.230
# test2   0.022   1.598
# test3   0.233   2.386
# test4  -0.162   0.809
# test5   0.417   1.602

"Coefficients for dffclt show the test was not so difficult. 

Regarding discrimation, except for question 4, all other question 
were very discriminating.
"

plot(IRT_model, type="ICC")

plot(IRT_model, type="ICC", items=c(1,3))

"Plot the information, which tells me which are in the
x-axis gives me more information in terms of discrimination 
power of the items (all items). This is important to show design the items
in a way that they focus more or less on certain parameter
configurations, which in the case of the example is 
ability. 

The plot shows the test information covers from 0 to 2 with peak on one 
standard deviation of the ability."

plot(IRT_model, type="IIC", items=0)

factors <- factor.scores.ltm(IRT_model)
factors
# Scoring Method: Empirical Bayes
# Factor-Scores for observed response patterns:
#     test1 test2 test3 test4 test5 Obs   Exp     z1 se.z1
# 1      0     0     0     0     0 271 320.220 -0.966 0.642
# 2      0     0     0     0     1  56  41.950 -0.475 0.481
# 3      0     0     0     1     0 159 164.245 -0.683 0.546
# 4      0     0     0     1     1  34  32.191 -0.305 0.438
# 5      0     0     1     0     0  80  31.855 -0.310 0.440
# 6      0     0     1     0     1  12  11.481 -0.033 0.400
# 7      0     0     1     1     0  19  28.424 -0.164 0.413
# 8      0     0     1     1     1   4  13.259  0.095 0.397
# 9      0     1     0     0     0  91  79.162 -0.476 0.481
# 10     0     1     0     0     1  21  21.535 -0.163 0.413
# 11     0     1     0     1     0  90  60.698 -0.306 0.439
# 12     0     1     0     1     1  51  22.033 -0.030 0.400
# 13     0     1     1     0     0  12  21.621 -0.034 0.400
# 14     0     1     1     0     1   3  12.806  0.220 0.402
# 15     0     1     1     1     0   7  24.954  0.094 0.397
# 16     0     1     1     1     1   9  18.947  0.355 0.417
# 17     1     0     0     0     0  64  40.740 -0.158 0.413
# 18     1     0     0     0     1  17  19.213  0.100 0.397
# 19     1     0     0     1     0  69  41.880 -0.025 0.400
# 20     1     0     0     1     1  19  25.218  0.229 0.403
# 21     1     0     1     0     0  31  24.556  0.225 0.403
# 22     1     0     1     0     1   6  24.412  0.507 0.443
# 23     1     0     1     1     0  31  36.512  0.361 0.418
# 24     1     0     1     1     1  37  48.471  0.679 0.481
# 25     1     1     0     0     0  24  36.160  0.100 0.397
# 26     1     1     0     0     1  15  27.767  0.362 0.418
# 27     1     1     0     1     0  22  47.432  0.228 0.403
# 28     1     1     0     1     1  34  47.495  0.511 0.444
# 29     1     1     1     0     0  58  45.856  0.506 0.443
# 30     1     1     1     0     1  82  83.555  0.881 0.534
# 31     1     1     1     1     0  71  90.979  0.678 0.481
# 32     1     1     1     1     1 289 242.373  1.144 0.610


"Factor scores shows tha the most frequent combination (Obs) were
all wrong (271), all correct (289), and only test_4 correct (159)
"

hist(factors$score.dat$z1 )
hist(df_E2$qualification_score)

#----------------------------------------------------------------
"Merge this with the Session data from E2"

df_score.dat <- data.frame(factors$score.dat)

#Convert to double to be able to join with the fields tes1..5 of df_score.dat
df_score.dat$test1 <- as.factor(df_score.dat$test1)
df_score.dat$test2 <- as.factor(df_score.dat$test2)
df_score.dat$test3 <- as.factor(df_score.dat$test3)
df_score.dat$test4<- as.factor(df_score.dat$test4)
df_score.dat$test5 <- as.factor(df_score.dat$test5)

#LEFT JOIN to associate the new difficulty scores (z1) to the partipants.
df_new <- left_join(df_E2,df_score.dat,by=c("test1"="test1","test2"="test2","test3"="test3","test4"="test4","test5"="test5"))


#Store in the original file the new difficulty scores (z1) of the partipants
write.csv(df_new,"C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data//irt//E2_QualificationTest_IRT.csv")

#Visualizing the results
plot(df_new$years_programming, df_new$z1)
title("Factor Scores by Years of Programming - E2")
plot(df_new$years_programming, df_new$qualification_score)
title("Average Scores by Years of Programming - E2")

cor.test(df_new$years_programming, df_new$z1)
#cor=0.3082745, t = 13.695, df = 1786, p-value < 2.2e-16
cor.test(df_new$years_programming, df_new$qualification_score)
#cor =0.321539 t = 14.351, df = 1786, p-value < 2.2e-16

#Correlation did not change and continue to be significant

#----------------------------------------------------------------

