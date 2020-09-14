"
Nearest Neighbor matching

TODO 
- Evaluate and visualize imbalacne see - https://imai.fas.harvard.edu/research/files/matchit.pdf
- run the genetic and optimal matching
- plan to do wiht my data (which confounders? Assume initially only skill and task complexity
- treatment is bug type (buggy or not buggy)
"

#install.packages("tableone")
#install.packages("Matching")
#install.packages("MatchIt")

library(tableone)
library(Matching)
#Now load the lalonde data (which is in the MatchIt package):
library(MatchIt)
library(dplyr)

data(lalonde)

df <- lalonde %>% mutate_at(
                      c("age", "educ","married", "black","nodegree",
                        "hispan","re74", "re75"), 
                      ~(scale(.) %>% as.vector)
                      ) 
#difference between the means divided by the square-root of the sum
#sample variances divided by 2
married_1 = df[df$treat==1,]$married
married_0 = df[df$treat==0,]$married

avg_married_1 = mean(married_1)
avg_married_0 = mean(married_0)
var_married_1 = var(married_1)
var_married_0 = var(married_0)


(avg_married_0 - avg_married_1) / (sqrt((var_married_0+var_married_1)/2))
#[1] 0.719492

#-------------------------------------
#Do Matching using Nearest Neighbor

set.seed(931139)
#Match on the propensity score itself, not logit of the propensity score.
#Obtain the standardized differences for the matched data.

m.out <- matchit(treat ~ age+educ+married+black+hispan+nodegree+re74+re75, 
                 data=df, method="nearest", distance="logit")
#xvars <- c("age", "educ","married","nodegree", "black","hispan","re74", "re75","re78") 

#df_matched <- df[unlist(m.data[c("treat","control")]),]


print(m.out)
summary(m.out)

m.data <- match.data(m.out)

re78_1 <- m.data[m.data$treat==1,]$re78
re78_0 <- m.data[m.data$treat==0,]$re78
t.test(re78_1,re78_0,paired = TRUE)

t.test(m.data[m.data$treat==1,"re78"],m.data[m.data$treat==0,"re78"])

# Paired t-test
# 
# data:  re78_1 and re78_0
# t = 1.2043, df = 184, p-value = 0.23 >>>>>NOT SIGNIFICANT
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -579.6904 2396.0948
# sample estimates:
#   mean of the differences 
# 908.2022 

plot(m.out)

#Compute standardized difference
df_matched <- MatchIt::match.data(matchModel)
avg_married_1 <- mean(df_matched[df_matched$treat==1,"married"])
avg_married_0 <- mean(df_matched[df_matched$treat==0,"married"])
var_married_1 <- var(df_matched[df_matched$treat==1,"married"])
var_married_0 <- var(df_matched[df_matched$treat==0,"married"])


(avg_married_1 - avg_married_0) / (sqrt((var_married_0+var_married_1)/2))
#0.05392746

