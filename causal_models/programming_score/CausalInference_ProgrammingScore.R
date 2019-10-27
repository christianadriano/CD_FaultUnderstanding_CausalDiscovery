"
Causal inference models of the programming test score.
"

library(rethinking)
library(stringr)

install.packages("ggm")
install.packages("ggdag")
library(ggdag)
library( dagitty )
library (ggm)


#Load data
path <- "C://Users//Christian//Documents//GitHub//Complexity_Metrics//output//"
dataset_E2 <- read.csv(str_c(path, "merged_tasks_complexity_E2.csv"))
df_E2 <- data.frame(dataset_E2)

#Causal graph
#Create
dag1.1 <- dagitty( "dag {
Prof -> Score
YoE -> Score
YoE -> Prof
}")

#Plot
coordinates(dag1.1) <- list( x=c(YoE=0,Score=1,Prof=2) , y=c(YoE=0,Score=1,Prof=0) )
plot( dag1.1 )
tidy_dagitty(dag1.1)
ggdag(dag1.1, layout = "circle")
 
#Conditional independence assumptions
paths(dag1.1,c("YoE"),"Score",directed = TRUE)
# $paths [1] "YoE -> Prof -> Score" "YoE -> Score"        
# $open [1] TRUE TRUE

#Simulate priors

#Build causal models

#Model-1 No interactions

#Model-2 Interaction between profession and yoe

#Model-3 Interaction between profession and yoe and gender

#Model-4 Add Duration
