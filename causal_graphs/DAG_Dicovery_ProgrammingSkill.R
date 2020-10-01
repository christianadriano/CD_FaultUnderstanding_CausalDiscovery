
"
Causal discovery for the programming skill related variables

profession [exogenous];
years_programming [exogenous];
age [exogenous]
qualification_score [outcome];
file_name [exogenous] work as block, because programmers were tested for each file_name
need to consider unique worker-id, i.e., programmer took a single qualification test, even
if they have taken multiple tasks.
"  
install.packages("pcalg")
install.packages("graph")
library(pcalg)

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("graph")
