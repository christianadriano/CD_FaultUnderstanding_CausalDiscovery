"Hobbyist 
Intervention on test duration to falsify or estimate 
the effect on the adjusted qualification score

There are 4 causal graphs for Hobbyist that stem from two causal discovery methods (Score-based
and Constraint-based) and two groups of users slow and fast responders.

We probe each of these four causal graphs by formulating three types of interventions: 
test duration (TD), years of programming experience (YoE), and age (A).

Each formultion consists of a conditional expectation that we approximate by performing a 
multilevel regression using Markov Chain Monte Carlo API. This API is available in the 
probabilistic programming language STAN which we interface using the R Rethinking package.

"

library(rethinking)
library(stringr)
library(dplyr)

#Load data
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//load_consent_create_indexes_E2.R")
df_E2$profession_id <- as.integer(df_E2$profession_id)

#-----------------------------
#Test Duration

#Constraint-Based Graph


#Score-Based Graph




#-----------------------------
#Years of Programming

#-----------------------------
#Age


