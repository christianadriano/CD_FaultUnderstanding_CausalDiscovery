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

adjustmentSets(dag1.1,exposure = "Prof",outcome = "Score",effect = c("direct"))
#{ YoE } because YoE is a confounder.

"The effect of YoE on Score is influenced by the Profession, i.e., YoE in different
professions have different effects on Score. In this sense, Profession adjusts the
effect of YoE on Score. The adjustment can be via mediation and interaction (a.k.a. moderation). 

In the graph model, we can only show mediation. To investigate interaction, we need 
to build models with interaction. 

The opposite can also be true, i.e., the effect of profession on score is influenced
by the YoE, which is also a confounder. This means that to intervene in the profession to 
evaluate Score, I would need to control for YoE. 

What is interesting to know is how much of the effect of YoE on Score is:
1- additive (direct effect)
2- additive mediated (indirect effect)
3- via interacting with profession

Note the assymetry, in a sense that a person cannot easily change Profession, but can increase YoE.

The relation between YoE and Profession is also not given. We need to confirm it.
I cannot condition on the Score, because it is a collider, which as such, it opens
the path between YoE and Prof.

"

#Simulate priors

#Build causal models

#Model-1 No interactions

#Model-2 Interaction between profession and yoe

#Model-3 Interaction between profession and yoe and gender

#Model-4 Add Duration
