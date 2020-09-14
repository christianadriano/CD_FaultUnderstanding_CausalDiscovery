"
Propensity score matching
"

library(tableone)
library(Matching)
#Now load the lalonde data (which is in the MatchIt package):
library(MatchIt)

library(dplyr)

data(lalonde)

#Center (mean==0) and scale (0,1) the confounding covariates
df <- lalonde %>% mutate_at(
                      c("age", "educ","married","nodegree", "black","hispan","re74", "re75"), 
                      ~(scale(.) %>% as.vector)
                      ) 
#----------------------------------------

#For the column married, find the standardized differences: 
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


#------------------------------------
#Compute Propensity Score

psmodel <- glm(treat ~ age+educ+married+black+hispan+nodegree+re74+re75, 
               family=binomial(),data=df)

min(psmodel$fitted.values)
max(psmodel$fitted.values)

hist(psmodel$fitted.values)

#-------------------------------------
#Do Matching on the Propensity Score

logit<-function(x) log(x/(1-x))

set.seed(931139)
#Match on the propensity score itself, not logit of the propensity score.
#Obtain the standardized differences for the matched data.

psmatch <- Match(Tr=df$treat,M=1,X=psmodel$fitted.values, replace=FALSE)
matched <- df[unlist(psmatch[c("index.treated","index.control")]),]
xvars <- c("age", "educ","married","nodegree", "black","hispan","re74", "re75") 

matchedtab1 <- CreateTableOne(var=xvars,strata = "treat",
                              data=matched,test=FALSE)

print(matchedtab1, smd=TRUE)
#SMD is the standard difference

#                        Stratified by treat
#                         0            1            SMD   
# n                      185          185              
# age (mean (SD))      -0.22 (1.06) -0.16 (0.72)  0.069
# educ (mean (SD))      0.11 (1.01)  0.03 (0.76)  0.085
# married (mean (SD))  -0.41 (0.83) -0.46 (0.80)  0.054
# nodegree (mean (SD))  0.04 (0.99)  0.16 (0.94)  0.127
# black (mean (SD))     0.15 (1.02)  0.91 (0.74)  0.852
# hispan (mean (SD))    0.32 (1.29) -0.18 (0.74)  0.479
# re74 (mean (SD))     -0.34 (0.65) -0.38 (0.75)  0.062
# re75 (mean (SD))     -0.17 (0.80) -0.20 (0.98)  0.034

#Compute standardized difference
avg_married_1 <- matchedtab1$ContTable$`1`["married","mean"]
avg_married_0 <- matchedtab1$ContTable$`0`["married","mean"]
sd_married_1 <- matchedtab1$ContTable$`1`["married","sd"]
sd_married_0 <- matchedtab1$ContTable$`0`["married","sd"]

(avg_married_1 - avg_married_0) / (sqrt((sd_married_0^2+sd_married_1^2)/2))

#-----------------------------
#Now using Caliper
set.seed(931139)
psmatch <- Match(Tr=df$treat,M=1,X=psmodel$fitted.values, 
                 caliper = 0.1,
                 replace=FALSE)
matched <- df[unlist(psmatch[c("index.treated","index.control")]),]
xvars <- c("age", "educ","married","nodegree", "black","hispan","re74", "re75") 

matchedtab1 <- CreateTableOne(var=xvars,strata = "treat",
                              data=matched,test=FALSE)

print(matchedtab1, smd=TRUE)
#                         Stratified by treat
#                          0            1            SMD   
# n                      111          111              
# age (mean (SD))      -0.11 (1.12) -0.12 (0.73)  0.006
# educ (mean (SD))      0.04 (1.01) -0.01 (0.88)  0.047
# married (mean (SD))  -0.35 (0.87) -0.35 (0.87) <0.001
# nodegree (mean (SD))  0.06 (0.99)  0.04 (0.99)  0.019
# black (mean (SD))     0.66 (0.92)  0.70 (0.90)  0.040
# hispan (mean (SD))   -0.03 (0.97) -0.06 (0.93)  0.029
# re74 (mean (SD))     -0.29 (0.73) -0.36 (0.89)  0.086
# re75 (mean (SD))     -0.07 (0.96) -0.29 (0.93)  0.239