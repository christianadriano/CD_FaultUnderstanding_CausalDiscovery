"IPW - Inverse Probability Weights"

library(tableone)
#install.packages("ipw")
#install.packages("sandwich") #for robust estimation
library(ipw)
library(sandwich)
library(survey)

load(url("http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/rhc.sav"))
View(rhc)
