"Check imbalances in Standardized Mean Differences - SMD

plotting results with Cobalt
https://cran.rstudio.com/web/packages/cobalt/vignettes/cobalt_A4_love.plot.html

Treatment: Bug or NoBug
Analyze imbalance for each Task (HIT01 to HIT08)
"

"Load data with treatment field (isBugCovering) and ground truth (answer correct)"
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_ground_truth_E2.R")
#summary(df_E2_ground)

source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//create_indexes_E2.R")

df_E2_ground<- run(df_E2_ground)

"Determine the covariates that need to taken into account w.r.t. imbalance.
The covariates are the ones that are confounders of the treatment (bug/nobug) and the 
effect (accuracy of the task)"

#see DAG in file DAG_ExogenousIntervention.R

"The covariates that can imbalance the intervention are:
- qualification_score
- profession
- volume_Halstead

We could evaluate balance by each file_name, which is a separate experiment.
"

df_E2_ground <-
  select(df_E2_ground,
         'qualification_score',
         'volume_Halstead',
         'profession',
         'isBugCovering',
          'file_name');

xvars <- c("qualification_score",
           "volume_Halstead",
           "profession"
          );

df2_fileName <- df_E2_ground[df_E2_ground$file_name=="HIT01_8",]

library(tableone)
library(Matching)
#Now load the lalonde data (which is in the MatchIt package):
library(MatchIt)
#Without Matching
raw_table <- CreateTableOne(vars=xvars,strata="isBugCovering",data=df_E2_ground,test=FALSE)
print(raw_table, smd=TRUE)
