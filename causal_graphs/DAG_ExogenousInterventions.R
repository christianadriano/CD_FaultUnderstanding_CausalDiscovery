"
Causal graph of exogenous interverntions (do not include task execution covariates)
"

"Load data with treatment field (isBugCovering) and ground truth (answer correct)"
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_ground_truth_E2.R")
#summary(df_E2_ground)

"Determine the covariates that need to taken into account w.r.t. imbalance.
The covariates are the ones that are confounders of the treatment (bug/nobug) and the 
effect (accuracy of the task)"

library(ggdag)
library( dagitty )
library (ggm)

#Causal graph
dag2 <- dagitty( "dag {
isAnswerCorrect [outcome];
isBugCovering [exogenous];
volume_Halstead [exogenous];
profession [exogenous];
qualification_score [endogenous];
file_name [exogenous];
volume_Halstead -> isAnswerCorrect;
profession -> qualification_score;
qualification_score -> isAnswerCorrect;
profession -> isAnswerCorrect;
isBugCovering -> isAnswerCorrect;
file_name -> isAnswerCorrect;
}")

#coordinates(dag2) <- list( x=c(upvotes=0,length=2,type=2,rank=2,replies=4) ,
#                           y=c(upvotes=3,length=1,type=2,rank=0,replies=3))

plot( graphLayout(dag2,method="spring"))
tidy_dagitty(dag2)
ggdag(dag2, layout = "circle")

condIndep <- impliedConditionalIndependencies(dag2)
condIndep #{} 

# file_name _||_ isBugCovering
# file_name _||_ profession
# file_name _||_ qualification_score
# file_name _||_ volume_Halstead
# isBugCovering _||_ profession
# isBugCovering _||_ qualification_score
# isBugCovering _||_ volume_Halstead
# profession _||_ volume_Halstead
# qualification_score _||_ volume_Halstead
#All of these independencies hold, because they are all exogenous variables

#Qualification_score is affected by profession. (build the model here)
