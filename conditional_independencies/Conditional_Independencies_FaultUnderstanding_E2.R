"
Conditional Independencies implied by hypothetical Causal Graph for Task Accuracy (Experiment-2)

Results = 57 implied conditional independencies.

"
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
Explanation.Size [pos="0.061,-0.179"];
Difficulty [pos="-0.142,-0.179"];
Answer.Type -> Confidence;
Code.Complexity -> Difficulty;
Confidence -> Accuracy;
Duration -> Confidence;
Explanation.Size -> Accuracy;
Explanation.Size -> Duration;
Difficulty -> Duration;
Difficulty -> Explanation.Size;
Programmer.Skill -> Confidence;
Programmer.Skill -> Difficulty;
Task.Type -> Explanation.Size;
Task.Type -> Difficulty;
}'
graph <- dagitty(dag)
plot(graph)

pairs <- combn( names( graph), 2 )
impliedConditionalIndependencies(graph)

apply( pairs, 2, function(x){
  all.other.variables <- setdiff( names(graph), x )
  if( dseparated( graph, x[1], x[2], all.other.variables ) ){
    message( x[1]," _||_ ",x[2]," | ", 
             paste( all.other.variables, collapse=",") )
  }
} )

"Results = 57 implied conditional independencies

Accuracy _||_ Answer.Type | Cnfd, Duration, Programmer.Skill
Accuracy _||_ Answer.Type | Cnfd, Difficulty, Duration, Task.Type
Accuracy _||_ Answer.Type | Cnfd, Explanation.Size
Accuracy _||_ Code.Complexity | Difficulty, Programmer.Skill, Task.Type
Accuracy _||_ Code.Complexity | Difficulty, Explanation.Size, Programmer.Skill
Accuracy _||_ Code.Complexity | Duration, Explanation.Size, Programmer.Skill
Accuracy _||_ Code.Complexity | Cnfd, Difficulty, Duration, Task.Type
Accuracy _||_ Code.Complexity | Cnfd, Explanation.Size
Accuracy _||_ Difficulty | Duration, Explanation.Size, Programmer.Skill
Accuracy _||_ Difficulty | Cnfd, Explanation.Size
Accuracy _||_ Duration | Cnfd, Explanation.Size
Accuracy _||_ Programmer.Skill | Cnfd, Difficulty, Duration, Task.Type
Accuracy _||_ Programmer.Skill | Cnfd, Explanation.Size
Accuracy _||_ Task.Type | Difficulty, Explanation.Size, Programmer.Skill
Accuracy _||_ Task.Type | Duration, Explanation.Size, Programmer.Skill
Accuracy _||_ Task.Type | Cnfd, Explanation.Size
Answer.Type _||_ Code.Complexity
Answer.Type _||_ Difficulty
Answer.Type _||_ Duration
Answer.Type _||_ Explanation.Size
Answer.Type _||_ Programmer.Skill
Answer.Type _||_ Task.Type
Code.Complexity _||_ Cnfd | Duration, Programmer.Skill
Code.Complexity _||_ Cnfd | Difficulty, Explanation.Size, Programmer.Skill
Code.Complexity _||_ Cnfd | Difficulty, Programmer.Skill, Task.Type
Code.Complexity _||_ Duration | Difficulty, Explanation.Size
Code.Complexity _||_ Duration | Difficulty, Task.Type
Code.Complexity _||_ Explanation.Size | Difficulty, Task.Type
Code.Complexity _||_ Programmer.Skill
Code.Complexity _||_ Task.Type
Cnfd _||_ Difficulty | Duration, Programmer.Skill
Cnfd _||_ Explanation.Size | Difficulty, Duration, Task.Type
Cnfd _||_ Explanation.Size | Duration, Programmer.Skill
Cnfd _||_ Task.Type | Difficulty, Explanation.Size, Programmer.Skill
Cnfd _||_ Task.Type | Duration, Programmer.Skill
Duration _||_ Programmer.Skill | Difficulty, Task.Type
Duration _||_ Programmer.Skill | Difficulty, Explanation.Size
Duration _||_ Task.Type | Difficulty, Explanation.Size
Explanation.Size _||_ Programmer.Skill | Difficulty, Task.Type
Programmer.Skill _||_ Task.Type
Accuracy _||_ Answer.Type | Code.Complexity,Confidence,Difficulty,Duration,Explanation.Sizeanation,Programmer.Skill,Task.Type
Accuracy _||_ Code.Complexity | Answer.Type,Confidence,Difficulty,Duration,Explanation.Sizeanation,Programmer.Skill,Task.Type
Accuracy _||_ Difficulty | Answer.Type,Code.Complexity,Confidence,Duration,Explanation.Sizeanation,Programmer.Skill,Task.Type
Accuracy _||_ Duration | Answer.Type,Code.Complexity,Confidence,Difficulty,Explanation.Sizeanation,Programmer.Skill,Task.Type
Accuracy _||_ Programmer.Skill | Answer.Type,Code.Complexity,Confidence,Difficulty,Duration,Explanation.Sizeanation,Task.Type
Accuracy _||_ Task.Type | Answer.Type,Code.Complexity,Confidence,Difficulty,Duration,Explanation.Sizeanation,Programmer.Skill
Answer.Type _||_ Code.Complexity | Accuracy,Confidence,Difficulty,Duration,Explanation.Sizeanation,Programmer.Skill,Task.Type
Answer.Type _||_ Difficulty | Accuracy,Code.Complexity,Confidence,Duration,Explanation.Sizeanation,Programmer.Skill,Task.Type
Answer.Type _||_ Explanation.Sizeanation | Accuracy,Code.Complexity,Confidence,Difficulty,Duration,Programmer.Skill,Task.Type
Answer.Type _||_ Task.Type | Accuracy,Code.Complexity,Confidence,Difficulty,Duration,Explanation.Sizeanation,Programmer.Skill
Code.Complexity _||_ Confidence | Accuracy,Answer.Type,Difficulty,Duration,Explanation.Sizeanation,Programmer.Skill,Task.Type
Code.Complexity _||_ Duration | Accuracy,Answer.Type,Confidence,Difficulty,Explanation.Sizeanation,Programmer.Skill,Task.Type
Code.Complexity _||_ Explanation.Sizeanation | Accuracy,Answer.Type,Confidence,Difficulty,Duration,Programmer.Skill,Task.Type
Confidence _||_ Difficulty | Accuracy,Answer.Type,Code.Complexity,Duration,Explanation.Sizeanation,Programmer.Skill,Task.Type
Confidence _||_ Task.Type | Accuracy,Answer.Type,Code.Complexity,Difficulty,Duration,Explanation.Sizeanation,Programmer.Skill
Duration _||_ Task.Type | Accuracy,Answer.Type,Code.Complexity,Confidence,Difficulty,Explanation.Sizeanation,Programmer.Skill
Explanation.Sizeanation _||_ Programmer.Skill | Accuracy,Answer.Type,Code.Complexity,Confidence,Difficulty,Duration,Task.Type

"