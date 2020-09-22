"IPW - Inverse Probability Weights"

library(tableone)
#install.packages("ipw")
#install.packages("sandwich") #for robust estimation
library(ipw)
library(sandwich)
library(survey)
library(ggplot2)
#install.packages("hrbrthemes")
library(hrbrthemes)

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

covariate_names <- c("ARF","CHF","Cirr","colcan","Coma","lungcan","MOSF","sepsis","age","female","meanbp1","aps")


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
mydata$psvalue <- ps

summary(psmodel)
# Coefficients:
#                Estimate Std. Error z value Pr(>|z|)    
#   (Intercept) -0.7299670  0.1997692  -3.654 0.000258 ***
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

#Distribution of Propensity Score before Matching
m <- split(mydata$psvalue, mydata$treatment)

df_control <- data.frame(value=m$`0`)
df_treat <- data.frame(value=m$`1`)

p <- ggplot() + aes(x=value)+
  geom_histogram( data=df_control, aes(y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=0.6, y=2.5, label="control"), color="#69b3a2") +
  geom_histogram(data=df_treat,  aes(y = -..density..), fill= "#404080") +
  geom_label( aes(x=0.6, y=-3.5, label="treatment"), color="#404080") +
  theme_ipsum() +
  xlab("Propensity Score")
p
#Create the weights for treated and non-treated
weight <- ifelse(treatment==1,1/(ps),1/(1-ps))
#Apply the weights to the data
weighted_data <- svydesign(ids=~1,data=mydata,weights=~weight)


#weighted table 1
weightedtable = svyCreateTableOne(strata="treatment",
                                  data=weighted_data, test=FALSE)
print(weightedtable,smd=TRUE)
#                           Stratified by treatment
#                               0               1         SMD   
# n                         5732.49         5744.88               
# ARF (mean (SD))          0.44 (0.50)     0.44 (0.50)   0.010
# CHF (mean (SD))          0.08 (0.27)     0.08 (0.27)   0.005
# Cirr (mean (SD))         0.04 (0.19)     0.04 (0.19)   0.001
# colcan (mean (SD))       0.00 (0.04)     0.00 (0.06)   0.042
# Coma (mean (SD))         0.08 (0.26)     0.07 (0.25)   0.023
# lungcan (mean (SD))      0.01 (0.08)     0.01 (0.09)   0.014
# MOSF (mean (SD))         0.07 (0.26)     0.07 (0.26)   0.004
# sepsis (mean (SD))       0.21 (0.41)     0.22 (0.41)   0.002
# age (mean (SD))         61.36 (17.56)   61.43 (15.33)  0.004
# female (mean (SD))       0.45 (0.50)     0.45 (0.50)   0.001
# meanbp1 (mean (SD))     78.60 (37.58)   79.26 (40.31)  0.017
# aps (mean (SD))         52.91 (19.30)   58.56 (19.84)  0.289
# treatment (mean (SD))    0.00 (0.00)     1.00 (0.00)     Inf
# died (mean (SD))         0.63 (0.48)     0.68 (0.47)   0.109

#Compute the reweighted value for a single covariate
#in the case below it will be the age for treated (61.43)
mean(weight[treatment==1]*age[treatment==1])/mean(weight[treatment==1])
#[1] 61.42933
#the weight vector has the propensity scores for each row
#eventhough the formula involves a sum both in the denomintaor and the numerator,
#since they the number of elements is the same (n), the formula 
#can be simplified by the ratio of the means.

#-----------------------------------------------------------------
"MARGINAL STRUTURAL MODEL
Intervention (treatment) is rhc vs not
Outcome Y is the variable 'died' yes/no
g() is the link function
Two steps here:
1- use the log link to get the causal relative risk
2- use an identity link to get the causal risk difference

"
#-----------------
# Relative Causal Risk

#Model to fit: 
# Log(Yt) = intercept + treatment*coeff


glm.obj <- glm(died~treatment,weights=weight, family = binomial(link="logit"))
summary(glm.obj) #shows the coefficient is statistically significan
betaiptw <- coef(glm.obj)
betaiptw
# (Intercept)   treatment 
# 0.5342068   0.2288326 


#to properly account for weightig we use here asymptoptic (sandwich lib) variance
#this is necessary to compensate for the fact that the pseudo-population is larger
#than the actual population. 
SE <- sqrt(diag(vcovHC(glm.obj,type="HC0")))
SE 
# (Intercept)   treatment 
# 0.03602006  0.06485999 

#now get point estimates and confidence interval for a relative risk,
#which is needed to exponentiate
treat_coefficient = betaiptw[2]
two_standar_errors = 1.96*SE[2] #i.e., 95% confidence interval
#this is because the outcome is a log function, so, to 
#to obtain the value back I just need to exponentiate
causal_relative_risk <- exp(treat_coefficient) 
lower_causal_bound <- exp(treat_coefficient - two_standar_errors)
upper_causal_bound <- exp(treat_coefficient + two_standar_errors)
c(lower_causal_bound,causal_relative_risk,upper_causal_bound)
# lower     mean      upper 
# 1.107059  1.257132  1.427548 

#So the 95% CI is between (1.10, 1,43)
# value greater than 1 means that there is higher risk of death for 
# the treated group

#-------------------------------------
#Now compute the risk differences

#Model to fit: 
# E(Yt) = intercept + treatment*coeff

glm.obj <- glm(died~treatment,weights=weight, family = binomial(link="identity"))
summary(glm.obj) #shows the coefficient is statistically significan
betaiptw <- coef(glm.obj)
betaiptw
# (Intercept)   treatment 
# 0.63046375  0.05154951 

SE <- sqrt(diag(vcovHC(glm.obj,type="HC0")))
SE 
# (Intercept)   treatment 
# 0.008391924 0.014396571 

#now get point estimates and confidence interval for a relative risk,
#which is needed to exponentiate
treat_coefficient = betaiptw[2]
two_standar_errors = 1.96*SE[2] #i.e., 95% confidence interval
#this is because the outcome is a log function, so, to 
#to obtain the value back I just need to exponentiate
causal_relative_risk <- treat_coefficient
lower_causal_bound <- treat_coefficient - two_standar_errors
upper_causal_bound <- treat_coefficient + two_standar_errors
c(lower_causal_bound,causal_relative_risk,upper_causal_bound)
# lower      mean       upper 
# 0.02333223 0.05154951 0.07976679 

#---------------------------------------------
#Another option is to use the iptw package

#First check the weights (propensity scores)

weightmodel <- ipwpoint(exposure = treatment, family= "binomial", link="logit",
                        denominator=~age+female+meanbp1+ARF+CHF+Cirr+colcan+
                          Coma+lungcan+MOSF+sepsis, data=mydata)

#numeric summary of weights
summary(weightmodel$ipw.weights)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.046   1.405   1.721   2.001   2.280  21.606 

#plot of weights
ipwplot(weights=weightmodel$ipw.weights, logscale=FALSE, 
        main="weights", xlim=c(0,22))

#Second, fit the marginal structural model (risk difference)
msm <- svyglm(died~treatment,design=svydesign(~1,weights=~weight,data=mydata))
coef(msm)
# (Intercept)   treatment 
# 0.63046375  0.05154951 

confint(msm)
#                 2.5 %     97.5 %
# (Intercept) 0.61401445 0.64691305
# treatment   0.02333029 0.07976873


#----------------------------------
#TRUNCATE WEIGHTS
#this might be necessary if there are weights that are too large
truncweight <- replace(weight,weight>10,10) #if greater than 10, replace with 10
#get causal risk difference
glm.obj <- glm(died~treatment,weights=truncweight, family = binomial(link="identity"))
summary(glm.obj) #shows the coefficient is statistically significan

#or using the IPW package
weightmodel <- ipwpoint(exposure = treatment, family= "binomial", link="logit",
                        denominator=~age+female+meanbp1+ARF+CHF+Cirr+colcan+
                        Coma+lungcan+MOSF+sepsis, data=mydata, 
                        trunc = 0.01 #truncates 1% and 99% percentile
                        ) 

#numeric summary of weights
summary(weightmodel$weights.trunc)
# Min.   1st Qu.  Median  Mean   3rd Qu.   Max. 
# 1.081   1.405   1.721   1.972   2.280   6.379 


ipwplot(weights=weightmodel$weights.trunc, logscale=FALSE, 
        main="weights", xlim=c(0,22))

mydata$wt <- weightmodel$weights.trunc
#Second, fit the marginal structural model (risk difference)
msm <- svyglm(died~treatment,design=svydesign(~1,weights=~wt,data=mydata))
coef(msm)
# (Intercept)   treatment 
#  0.63045533  0.05494865
confint(msm)
#                 2.5 %     97.5 %
# (Intercept) 0.61400659 0.64690406
# treatment   0.02822931 0.08166799
