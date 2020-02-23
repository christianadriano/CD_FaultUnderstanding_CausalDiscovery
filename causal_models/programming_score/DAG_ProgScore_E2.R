"
Generate and Plot DAG for the E2 programmer score test
"

library(ggdag)
library( dagitty )
library (ggm)
library(tidyr)
library(devtools)

dag_score <- ' dag {
Programmer.Score[Outcome];
Years.Experience;
Test.Duration;
Age;
Age->Years.Experience;
Age->Programmer.Score;
Age->Test.Duration;
Years.Experience->Programmer.Score;
Years.Experience->Test.Duration;
Test.Duration->Programmer.Score
}'

graph_score <- dagitty(dag_score)
coordinates(graph_score) <- list( 
                           x=c(Years.Experience=0,Test.Duration=1,Age=2,Programmer.Score=1) ,
                           y=c(Years.Experience=0,Test.Duration=1,Age=0,Programmer.Score=2))
plot(graph_score)

impliedConditionalIndependencies(graph_score)

node <-"Programmer.Score"
MB <- markovBlanket( graph_score, node )
cat("\n",
    "Node",  ":", node,"\n",
    "Markov Blanket",":",MB,"\n",
    "Independent Set", ":",setdiff(names(graph_score),c(MB,node)),"\n"
)
