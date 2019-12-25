"
Item response model of the programming test qualification E1
"

library(rethinking)
library(stringr)
library(dplyr)

library(ltm)
library(psych)
library(mirt)

source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//load_consent_create_indexes_E1.R")

"Remove participants for whom we did not take the qualification test" 
df <- df_E1[complete.cases(df_E1[,"qualification_score"]),] #left with 3699 rows

"Replace false for 0(zero) and true for one(1)"
df$test1_ <-  ifelse(df$test1=="true",1,0)
df$test2_ <-  ifelse(df$test2=="true",1,0)
df$test3_ <-  ifelse(df$test3=="true",1,0)
df$test4_ <-  ifelse(df$test4=="true",1,0)

df <- df %>% select(test1_,test2_,test3_,test4_)

IRT_model <- ltm(df ~ z1, IRT.param=TRUE)

IRT_model
        Dffclt    Dscrmn
test1_ 1.8085255 1.3342712
test2_ 0.6030424 0.3566581
test3_ 0.9294014 7.5319611
test4_ 2.3272393 0.2682696

"Coefficients for dffclt show the test was difficult. First an last question were more.

Regarding discrimation, first and third questions were the most discriminating.

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
#     test1_ test2_ test3_ test4_  Obs      Exp     z1 se.z1
# 1       0      0      0      0 1012 1106.012 -0.313 0.934
# 2       0      0      0      1  590  530.431 -0.083 0.914
# 3       0      0      1      0  124   97.857  1.087 0.294
# 4       0      0      1      1   76   74.007  1.111 0.307
# 5       0      1      0      0  878  777.594 -0.010 0.902
# 6       0      1      0      1  335  394.506  0.194 0.834
# 7       0      1      1      0  134  125.219  1.120 0.313
# 8       0      1      1      1   54   96.469  1.147 0.331
# 9       1      0      0      0  112   83.603  0.521 0.522
# 10      1      0      0      1   74   47.504  0.584 0.450
# 11      1      0      1      0   29   56.306  1.243 0.412
# 12      1      0      1      1   25   46.043  1.294 0.463
# 13      1      1      0      0   43   73.299  0.601 0.432
# 14      1      1      0      1   18   42.869  0.646 0.387
# 15      1      1      1      0   68   80.037  1.314 0.484
# 16      1      1      1      1  127   67.244  1.387 0.563

"Factor scores shows tha the most frequent combination (Obs) was
peple who did not get any questions correct"

