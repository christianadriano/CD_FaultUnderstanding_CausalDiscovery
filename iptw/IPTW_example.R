"IPW - Inverse Probability Weights"

library(tableone)
#install.packages("ipw")
#install.packages("sandwich") #for robust estimation
library(ipw)
library(sandwich)
library(survey)

load(url("http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/rhc.sav"))
View(rhc)

#create a dataset only with the following variables and convert them to numeric
ARF <- as.numeric(rhc$cat1=="ARF")
CHF <- as.numeric(rhc$cat1=="CHF")
Cirr <- as.numeric(rhc$cat1=="Cirrhosis")
colcan <- as.numeric(rhc$cat1=="Colon Cancer")
Coma <- as.numeric(rhc$cat1=="Coma")
COPD <- as.numeric(rhc$cat1=="COPD")
lungcan <- as.numeric(rhc$cat1=="Lung Cancer")
MOSF <- as.numeric(rhc$cat1=="MOSF w/Malignancy")
sepsis <- as.numeric(rhc$cat1=="MOSF w/Sepsis")
female <- as.numeric(rhc$sex=="Female")
died <- as.numeric(rhc$death=="Yes")
age <- rhc$age
treatment <- as.numeric(rhc$swang1=="RHC")
meanbp1 <- rhc$meanbp1
aps <- rhc$aps1

#new dataset
mydata <- cbind(ARF,CHF,Cirr,colcan,Coma,lungcan,MOSF,sepsis,
                age,female,meanbp1,aps,treatment,died)
mydata <- data.frame(mydata)

#propensity score model
psmodel <- glm(treatment~age+female+meanbp1+ARF+CHF+Cirr+colcan+
                 Coma+lungcan+MOSF+sepsis,
               family = binomial(link="logit"))

#value of the propensity score for each subject
ps <- predict(psmodel,type = "response")
summary(psmodel)
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -0.7299670  0.1997692  -3.654 0.000258 ***
#   age         -0.0031374  0.0017289  -1.815 0.069567 .  
#   female      -0.1697903  0.0583574  -2.909 0.003620 ** 
#   meanbp1     -0.0109824  0.0008217 -13.366  < 2e-16 ***
#   ARF          1.2931956  0.1487784   8.692  < 2e-16 ***
#   CHF          1.6804704  0.1715672   9.795  < 2e-16 ***
#   Cirr         0.5234506  0.2181458   2.400 0.016416 *  
#   colcan       0.0295468  1.0985361   0.027 0.978542    
#   Coma         0.7013451  0.1854937   3.781 0.000156 ***
#   lungcan     -0.0869570  0.5039331  -0.173 0.863000    
#   MOSF         1.3046587  0.1772705   7.360 1.84e-13 ***
#   sepsis       2.0433604  0.1545437  13.222  < 2e-16 ***
#   ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

