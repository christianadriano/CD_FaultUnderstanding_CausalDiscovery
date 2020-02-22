"
Implications on Accuracy by conditioning on its Markov Blanket (Experiment-2)

Claim:
Accuracy _||_ (Difficulty, Duration, Answer.Type) | MB(Accuracy), where  
MB(Accuracy) is the Markov Blanket of Accuracy which consists of the set {Confidence Explanation}.

This means that if we condition on Confidence and Explanation, then Accuracy is 
independent of Difficulty, Duration, Answer.Type and all of the exposure variables.

See file MarkovBlankets_Tasks_E2.R for implications of the Markov Blankets of other variables.

"
library(ggdag)
library( dagitty )
library (ggm)
library(tidyr)
library(devtools)

dag <- ' dag {
bb="-0.5,-0.5,0.5,0.5";
Accuracy [outcome,pos="0.061,-0.054"];
Programmer.Score [exposure,pos="-0.327,-0.120"];
Code.Complexity [exposure, pos="-0.327,-0.225"];
Confidence [pos="-0.142,-0.054"];
Duration [pos="-0.040,-0.140"];
Explanation [pos="0.061,-0.179"];
Difficulty [pos="-0.142,-0.179"];
Code.Complexity -> Accuracy;
Code.Complexity -> Difficulty;
Code.Complexity -> Confidence;
Confidence -> Accuracy;
Duration -> Confidence;
Explanation -> Accuracy;
Programmer.Score -> Accuracy;
Explanation -> Duration;
Difficulty -> Duration;
Difficulty -> Explanation;
Difficulty -> Confidence
Difficulty -> Accuracy;
Programmer.Score -> Confidence;
Programmer.Score -> Difficulty
}'

graph <- dagitty(dag)
plot(graph)
node <-"Accuracy"
MB <- markovBlanket( graph, node )
cat("\n",
    "Node",  ":", node,"\n",
    "Markov Blanket",":",MB,"\n",
    "Independent Set", ":",setdiff(names(graph),c(MB,node)),"\n"
)

#Node: Accuracy
#Markov Blanket: Code.Complexity Confidence Difficulty Explanation Programmer.Score
#Independent Set: Duration

"
Verifying Implications
To test all other implications we just need to fit a regression model that has the 
markov blanket plus the variable of interest. If the data and the model are
consistent with each other, then the coefficient for the variable of interest will 
be zero or the credible interval will cover zero.
"

"Build regression models where 
Accuracy <- Confidence + Explanation + Code.Complexity + Duration + Programmer.Skill + Difficulty + Task.Type + Answer.Type

Assumptions: Difficulty is continuous

Build one model for each of the four pairs of Answer.Type, Task.Type.

Label meannings
MB for Markov Blanket, ACC for accuracy, Bug for Task with Bug, Pos for Positive Answer (Yes's)
NBug for Task with No Bug, Neg for Negative Answer (No's or IDK's)

MB.ACC.Bug.Pos (accuracy of true positives)
MB.ACC.NBug.Neg (accuracy of true negatives)
MB.ACC.Bug.Pos (accuracy of false positives)
MB.ACC.NBug.Neg (accuracy of false negatives)





"
