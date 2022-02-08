"
d-Separation sets implied by hypothetical Causal Graph for Task Accuracy (Experiment-2)

For each pair of non-adjacent nodes in this graph, find a set of variables that d-separates that pair.
Note that this is a weaker conditions than the Markov Blanket (MB), as the MB finds the minimal set of variables that 
isolates (separates) on variable from all the others that are not part of the MB set.

The d-Separation is useful to find which variables must be conditioned on in order to 
block a path between two variables. This is the case when one wants to block a confounder.

Nonetheless, as the results show, one has to condition on almost all remaining variables
to d-separate two non-adjacent nodes. For this reason, we will only use backdoor and frontdoor paths 
to decide on the adjustment set necessary to avoid confounding when computing a graph query.

"
install.packages("dagitty")
library(ggdag)
library( dagitty )
library (ggm)
library(tidyr)
library(devtools)

dag <- ' dag {
bb="-0.5,-0.5,0.5,0.5";
Accuracy [outcome,pos="0.061,-0.054"];
Programmer.Skill [exposure,pos="-0.327,-0.120"];
Task.Type [exposure,pos="-0.037,-0.225"];
Code.Complexity [exposure, pos="-0.327,-0.225"];
Answer.Type [pos="-0.327,-0.054"];
Confidence [pos="-0.142,-0.054"];
Duration [pos="-0.037,-0.120"];
Explanation [pos="0.061,-0.179"];
Difficulty [pos="-0.142,-0.179"];
Answer.Type -> Confidence;
Code.Complexity -> Difficulty;
Confidence -> Accuracy;
Duration -> Confidence;
Explanation -> Accuracy;
Explanation -> Duration;
Difficulty -> Duration;
Difficulty -> Explanation;
Programmer.Skill -> Confidence;
Programmer.Skill -> Difficulty;
Task.Type -> Explanation;
Task.Type -> Difficulty;
}'
graph <- dagitty(dag)
plot(graph)

pairs <- combn( names( graph), 2 )
apply( pairs, 2, function(x){
  all.other.variables <- setdiff( names(graph), x )
  if( dseparated( graph, x[1], x[2], all.other.variables ) ){
    message( x[1]," _||_ ",x[2]," | ", 
             paste( all.other.variables, collapse=",") )
  }
} )

"Results

Accuracy _||_ Answer.Type | Code.Complexity,Confidence,Difficulty,Duration,Explanation,Programmer.Skill,Task.Type
Accuracy _||_ Code.Complexity | Answer.Type,Confidence,Difficulty,Duration,Explanation,Programmer.Skill,Task.Type
Accuracy _||_ Difficulty | Answer.Type,Code.Complexity,Confidence,Duration,Explanation,Programmer.Skill,Task.Type
Accuracy _||_ Duration | Answer.Type,Code.Complexity,Confidence,Difficulty,Explanation,Programmer.Skill,Task.Type
Accuracy _||_ Programmer.Skill | Answer.Type,Code.Complexity,Confidence,Difficulty,Duration,Explanation,Task.Type
Accuracy _||_ Task.Type | Answer.Type,Code.Complexity,Confidence,Difficulty,Duration,Explanation,Programmer.Skill
Answer.Type _||_ Code.Complexity | Accuracy,Confidence,Difficulty,Duration,Explanation,Programmer.Skill,Task.Type
Answer.Type _||_ Difficulty | Accuracy,Code.Complexity,Confidence,Duration,Explanation,Programmer.Skill,Task.Type
Answer.Type _||_ Explanation | Accuracy,Code.Complexity,Confidence,Difficulty,Duration,Programmer.Skill,Task.Type
Answer.Type _||_ Task.Type | Accuracy,Code.Complexity,Confidence,Difficulty,Duration,Explanation,Programmer.Skill
Code.Complexity _||_ Confidence | Accuracy,Answer.Type,Difficulty,Duration,Explanation,Programmer.Skill,Task.Type
Code.Complexity _||_ Duration | Accuracy,Answer.Type,Confidence,Difficulty,Explanation,Programmer.Skill,Task.Type
Code.Complexity _||_ Explanation | Accuracy,Answer.Type,Confidence,Difficulty,Duration,Programmer.Skill,Task.Type
Confidence _||_ Difficulty | Accuracy,Answer.Type,Code.Complexity,Duration,Explanation,Programmer.Skill,Task.Type
Confidence _||_ Task.Type | Accuracy,Answer.Type,Code.Complexity,Difficulty,Duration,Explanation,Programmer.Skill
Duration _||_ Task.Type | Accuracy,Answer.Type,Code.Complexity,Confidence,Difficulty,Explanation,Programmer.Skill
Explanation _||_ Programmer.Skill | Accuracy,Answer.Type,Code.Complexity,Confidence,Difficulty,Duration,Task.Type

"