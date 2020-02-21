"
Markov Blankets for all Variables in the Full causal graph of Bug inspection accuracy (Experiment-2)

Compute the Markov Blankets for all variables in the graph.
The Markov Blanket (MB) of a variable X is the set of variables that 
make X independent of all other variables in the graph.

This means that x _||_ Y | MB(x) for Y not in MB(x)

The Markov Blanket of x allows to discover the smallest set of variables that would yield 
an approximate estimate (it can still be biases) of x.
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

for( v in names( graph ) ){
  cat(v, ":", markovBlanket( graph, v ), "\n" )
}
"

Accuracy : Confidence Explanation 
Answer.Type : Confidence Duration Programmer.Skill 
Code.Complexity : Difficulty Programmer.Skill Task.Type 
Confidence : Answer.Type Duration Programmer.Skill Accuracy Explanation 
Difficulty : Code.Complexity Programmer.Skill Task.Type Duration Explanation 
Duration : Difficulty Explanation Confidence Answer.Type Programmer.Skill 
Explanation : Difficulty Task.Type Accuracy Duration Confidence 
Programmer.Skill : Confidence Difficulty Answer.Type Duration Code.Complexity Task.Type 
Task.Type : Difficulty Explanation Code.Complexity Programmer.Skill 

Implication-1
This means that if we condition on Confidence and Explanation, then Accuracy is 
independent of Difficulty, Duration, Answer.Type and all of the exposure variables.

Implication-2
Given its Markov Blanket, the Answer.Type is conditionally independent of Accuracy, 
Difficulty, Explanation size, Code Complexity, Task.Type.

This means if we change the answer type from YES to NO, but we keep the Markov Blanket with 
the same values (on average), then there we will see not change in the Accuracy, 
Difficulty, Explanation size of these answers for a task with similar Code Complexity and 
same Task.Type. i.e., choosing between Yes or No has not effect on Accuracy if
we do not change Difficulty, Duration or and Programmer.Skill.

Implication-3
Given its Markov Blanket, the Code Complexity is conditionally independent of Accuracy,
Confidence, Explanation, Duration, and Answer Type.

This means if we change the complexity of the code, but we keep the 
Markov Blanket (Difficulty Programmer.Skill Task.Type) with 
the same values (on average), then there we will see not change in the Accuracy,
Confidence, Explanation, Duration, and Answer Type of these answers for a 
task with the same Task.Type. i.e., choosing between more or less complex code 
should have no effect on Accuracy if
we do not change Difficulty Programmer.Skill Task.Type. 

So, if we give the same programmer a more complex code that is also buggy (task type) and
if the programmer finds the task as difficult as the previous one, we should expect that
the accuracy of the programmer's answer will also be the same.

Implications 4,5,6, and 7 deal with variables that are challenging to intervene in our
our specified domain, because these are dependent variables. This does not mean that one
can still imagine ways to keep confidence, difficulty, explanation size, and duration unchanged 
while still changing variables outside of the Markov Blanket of these. We simply do not
have any plausible scenario to do so.

Implication-8
Given its Markov Blanket, the Programmer.Skill is conditionally independent of Accuracy, 
Explanation, and Answer.Type 

This means if we change the answer type from YES to NO, but we keep the Markov Blanket with 
the same values (on average), then there we will see not change in the Accuracy, Duration, 
or Confidence of these answers. i.e., choosing between Yes or No has not effect on Accuracy if
we do not change Difficulty, Explanation, Code.Complexity, and Programmer.Skill.

Implication-9
Given the Markov Blanket, the Task.Type is conditionally independent of Accuracy, 
Duration, and Confidence. 

This means if we change the task type from Bug to no-Bug, but we keep the Markov Blanket with 
the same values (on average), then there we will see not change in the Accuracy, Duration, 
or Confidence of these tasks i.e., delegating a bug or a no-bug task has not effect 
on Accuracy of the task if we do not change Difficulty, Explanation, Code.Complexity, and Programmer.Skill.

Verifying Implications
To test all other implications we just need to fit a regression model that has the 
markov blanket plus the variable of interest. If the data and the model are
consistent with each other, then the coefficient for the variable of interest will 
be zero or the credible interval cover zero.
"
