
"
Causal discovery for the programming skill related factors

"  
"There are two packages in R for Bayesian Network Learning (or causal graph learning):
bnlearn and pcalg. I decided to use bnlearn, because its installation was easier than
pcalg. The pcalg requires the BiocManager (install.packages(BiocManager)), which is
very larger framework, which is not installing smoothly"

#-----------------------------------
#Example with BNLEARN (will move it to the bottom later)
#install.packages("bnlearn")
#
library(bnlearn)
data(learning.test)
str(learning.test)
bn.gs <- gs(learning.test)
bn.gs
plot(bn.gs)
#-----------------------------------            
"APPLY TO THE PROGRAMMING SKILL FACTORS 

profession [exogenous];
years_programming [exogenous];
age [exogenous]
gender [exogenous]
qualification_score [outcome];
file_name [exogenous] work as block, because programmers were tested for each file_name
need to consider unique worker-id, i.e., programmer took a single qualification test, even
if they have taken multiple tasks.
" 
#install.packages("tidyverse")
"Load data with treatment field (isBugCovering) and ground truth (answer correct)"
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_ground_truth_E2.R")
#summary(df_E2_ground)

source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//create_indexes_E2.R")
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//util//Multiplot.R")
df_E2_ground<- run(df_E2_ground)

library(dplyr)
df_selected <-
  dplyr::select(df_E2_ground,profession, 
                years_programming,
                qualification_score,
                age,
                profession,
                gender
  );

#
bn.gs <- gs(df_selected[df_selected$profession=="Other",])
plot(bn.gs)

#Constraint
bn.pc <- pc.stable(df_selected)
plot(bn.pc)

#Hill-Climbing algorithm
professions = c("Other", "Undergraduate_Student","Graduate_Student","Hobbyist","Profession")
plot_vector = vector("list",length(professions))
for (i in 1:length(professions)) {
  choice = professions[i]
  bn.hc <- hc(df_selected[df_selected$profession==choice,])
  plot_vector[i] <-  plot(bn.hc,main=choice);
}
multiplot(plot_vector, col=2)
