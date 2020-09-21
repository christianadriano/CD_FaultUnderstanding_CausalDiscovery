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

ARF <- as.numeric(rfc$cat1=="ARF")
CHF <- as.numeric(rfc$cat1=="CHF")
Cirr <- as.numeric(rfc$cat1=="Cirrhoris")
colcan <- as.numeric(rfc$cat1=="Colon Cancer")