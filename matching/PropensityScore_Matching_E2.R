"
Propensity score matching
"

install.packages("tableone")
install.packages("Matching")
install.packages("MatchIt")

library(tableone)
library(Matching)

#Now load the lalonde data (which is in the MatchIt package):
library(MatchIt)

data(lalonde)
