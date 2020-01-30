"
Item response model of the programming test qualification E2
"
library(dplyr)
library(ltm)
library(psych)
library(mirt)

source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//load_consent_create_indexes_E2.R")

"Remove participants for whom we did not take the qualification test" 
df <- df_E2[complete.cases(df_E2[,"qualification_score"]),] #left with 1438 rows

"Replace false for 0(zero) and true for one(1)"
df$test1_ <-  ifelse(df$test1=="true",1,0)
df$test2_ <-  ifelse(df$test2=="true",1,0)
df$test3_ <-  ifelse(df$test3=="true",1,0)
df$test4_ <-  ifelse(df$test4=="true",1,0)
df$test5_ <-  ifelse(df$test5=="true",1,0)

df <- df %>% dplyr::select(test1_,test2_,test3_,test4_,test5_)

write.csv(df,"C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//E2_QualificationTestResults.csv")


IRT_model <- ltm(df ~ z1, IRT.param=TRUE)

IRT_model
# Coefficients:
#          Dffclt  Dscrmn
# test1_   0.126   2.997
# test2_   0.173   1.572
# test3_   0.330   2.067
# test4_   0.071   0.700
# test5_   0.606   1.598

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

factor.scores.ltm(IRT_model)
# Scoring Method: Empirical Bayes
# Factor-Scores for observed response patterns:
# Factor-Scores for observed response patterns:
#   test1_ test2_ test3_ test4_ test5_ Obs     Exp     z1 se.z1
# 1       0      0      0      0      0 241 287.560 -0.898 0.659
# 2       0      0      0      0      1  46  32.376 -0.375 0.501
# 3       0      0      0      1      0 145 143.202 -0.633 0.575
# 4       0      0      0      1      1  27  23.433 -0.214 0.464
# 5       0      0      1      0      0  77  35.402 -0.264 0.474
# 6       0      0      1      0      1  12  10.393  0.054 0.428
# 7       0      0      1      1      0  18  27.936 -0.117 0.447
# 8       0      0      1      1      1   4  10.582  0.180 0.423
# 9       0      1      0      0      0  79  65.801 -0.382 0.502
# 10      0      1      0      0      1  14  15.896 -0.038 0.436
# 11      0      1      0      1      0  76  47.385 -0.219 0.465
# 12      0      1      0      1      1  28  15.012  0.091 0.426
# 13      0      1      1      0      0  12  20.840  0.049 0.428
# 14      0      1      1      0      1   3  10.687  0.338 0.429
# 15      0      1      1      1      0   6  21.135  0.175 0.423
# 16      0      1      1      1      1   7  13.830  0.470 0.442
# 17      1      0      0      0      0  58  38.416 -0.072 0.440
# 18      1      0      0      0      1  16  15.773  0.221 0.424
# 19      1      0      0      1      0  54  35.324  0.060 0.427
# 20      1      0      0      1      1  14  18.477  0.348 0.430
# 21      1      0      1      0      0  30  23.896  0.306 0.427
# 22      1      0      1      0      1   5  20.303  0.618 0.464
# 23      1      0      1      1      0  22  30.097  0.436 0.438
# 24      1      0      1      1      1  23  33.377  0.778 0.495
# 25      1      1      0      0      0  22  31.463  0.217 0.423
# 26      1      1      0      0      1  12  22.336  0.515 0.448
# 27      1      1      0      1      0  15  36.712  0.343 0.429
# 28      1      1      0      1      1  24  33.648  0.663 0.472
# 29      1      1      1      0      0  50  40.003  0.612 0.463
# 30      1      1      1      0      1  59  64.039  1.015 0.549
# 31      1      1      1      1      0  50  65.450  0.772 0.494
# 32      1      1      1      1      1 189 147.216  1.249 0.609

"Factor scores shows tha the most frequent combination (Obs) were
all wrong (241), all correct (189), and only test_4 correct (145)
