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

See example in script Examples_RethinkingPackage.R

"



#Load data
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E2.R")
df_E2 <- load_consent_create_indexes()
df_E2$profession_id <- as.integer(df_E2$profession_id)



#-----------------------------
#Test Duration
f <- alist(
  y ~ dnorm( mu , sigma ),
  mu ~ dnorm( 0 , 10 ),
  sigma ~ dexp( 1 )
)

#Constraint-Based Graph


#Score-Based Graph

#Evaluation

#what proportion of causal effects (coefficients) were significant?
#Accuracy of models
#Risk of overfitting


#-----------------------------
#Years of Programming

#-----------------------------
#Age

#-----------------------------
#Summary of comparisons

#Mean proportion of significant coefficients
#Mean Accuracy of models
#Mean Risk of overfitting




