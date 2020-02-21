"
Full Causal Graph for Bug inspection accuracy in the context of Experiment-2

Verification of the assumptions encoded in the grpah:

1- List of marginal independences (unconditional)
2- List of conditional independences
3- Adjustments to identify direct effect paths
4- Adjustments to identify total effect paths
5- Adjustments to identify indirect effect paths
6- Sensitivity analysis
6.1 Change of priors
6.2 Change of node edges
"
library(ggdag)
library( dagitty )
library (ggm)
library(tidyr)
library(devtools)

dag <- ' dag {
bb="-0.5,-0.5,0.5,0.5";
Accuracy [outcome,pos="0.061,-0.034"];
Programmer.Skill [exposure,pos="-0.327,-0.120"];
Task.Type [exposure,pos="-0.044,-0.233"];
Code.Complexity [exposure, pos="-0.327,-0.233"];
Answer.Type [pos="-0.327,-0.034"];
Confidence [pos="-0.142,-0.034"];
Duration [pos="-0.037,-0.095"];
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
tidy_dagitty(graph)
ggdag(graph, layout = "circle")
#check layout and color options:
#https://cran.r-project.org/web/packages/ggdag/vignettes/intro-to-ggdag.html

tidy_dagitty(graph, layout = "fr") %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_node(col="lightgrey") +
  geom_dag_text(col="black") +
  geom_dag_edges() +
  theme_dag()

impliedConditionalIndependencies(graph)


"INDEPENDENCIES IMPLIED BY THE CAUSAL GRAPH

After applying the d-separation criterion to all pairs of variables, we
obtained a set of independencies that are implied by the causal graph model."

"Marginal independencies
Some marginal independencies exist by definition, because the covariates are exposure
variables that were randomized in the experiment design.

Expected marginal independencies because of the randomized task design
Code.Complexity _||_ Task.Type

Expected marginal independencies because of the randomized task allocation
Answer.Type _||_ Code.Complexity
Answer.Type _||_ Difficulty
Answer.Type _||_ Duration
Answer.Type _||_ Explanation
Answer.Type _||_ Programmer.Skill
Answer.Type _||_ Task.Type
Code.Complexity _||_ Programmer.Skill
Programmer.Skill _||_ Task.Type

Conditional Independencies 

Related to the Outcome Variable
Below are equivalent sets of independencies. They are equivalent in a sense that 
the results (independence) should hold by conditioning on any of the different 
sets of covariates listed. These equivalences will be used to verify if the
graph against the data.

Accuracy _||_ Answer.Type | Confidence, Duration, Programmer.Skill
Accuracy _||_ Answer.Type | Confidence, Difficulty, Duration, Task.Type
Accuracy _||_ Answer.Type | Confidence, Explanation

Accuracy _||_ Code.Complexity | Difficulty, Programmer.Skill, Task.Type
Accuracy _||_ Code.Complexity | Difficulty, Explanation, Programmer.Skill
Accuracy _||_ Code.Complexity | Duration, Explanation, Programmer.Skill
Accuracy _||_ Code.Complexity | Confidence, Difficulty, Duration, Task.Type
Accuracy _||_ Code.Complexity | Confidence, Explanation

Accuracy _||_ Difficulty | Duration, Explanation, Programmer.Skill
Accuracy _||_ Difficulty | Confidence, Explanation

Accuracy _||_ Duration | Confidence, Explanation

Accuracy _||_ Programmer.Skill | Confidence, Difficulty, Duration, Task.Type
Accuracy _||_ Programmer.Skill | Confidence, Explanation

Accuracy _||_ Task.Type | Difficulty, Explanation, Programmer.Skill
Accuracy _||_ Task.Type | Duration, Explanation, Programmer.Skill
Accuracy _||_ Task.Type | Confidence, Explanation

Dependent variables related
Code.Complexity _||_ Confidence | Duration, Programmer.Skill
Code.Complexity _||_ Confidence | Difficulty, Explanation, Programmer.Skill
Code.Complexity _||_ Confidence | Difficulty, Programmer.Skill, Task.Type
Code.Complexity _||_ Duration | Difficulty, Explanation
Code.Complexity _||_ Duration | Difficulty, Task.Type
Code.Complexity _||_ Explanation | Difficulty, Task.Type

Confidence _||_ Difficulty | Duration, Programmer.Skill
Confidence _||_ Explanation | Difficulty, Duration, Task.Type
Confidence _||_ Explanation | Duration, Programmer.Skill
Confidence _||_ Task.Type | Difficulty, Explanation, Programmer.Skill
Confidence _||_ Task.Type | Duration, Programmer.Skill

Duration _||_ Programmer.Skill | Difficulty, Task.Type
Duration _||_ Programmer.Skill | Difficulty, Explanation
Duration _||_ Task.Type | Difficulty, Explanation

Explanation _||_ Programmer.Skill | Difficulty, Task.Type

"

"IMPLIED OPEN and CLOSED CAUSAL PATHS

A causal path is a chain of cause-effect dependencies that link the expousure variable with the outcome variable.
Each path contains a covariate only once. Causal paths can be opened or closed.

Opened means that exposure and outcome variables are d-connected through that path.
Closed means that exposure and outcome variables are d-separated through that path.

A path is closed if at least one of the covariates in the path is a collider node, otherwise,
the path is opened. A collider is node that has at least two parents.  
We can 'open' these paths by conditioning on the collider itself (i.e. on M, in this instance), 
or any covariate that is a 'descendant' of the collider.

Conditioning is necessary to close backdoor paths, which can bias the estimate of the 
effect of the exposure on the outcome. Assuming that we have a hypothesis about a set of
Z variables that we would like to condition on. The path p between exposure e and outcome o
will be closed in the following two situations:

the path p with a set of nodes x, z, y contains a chain x -> z -> y or a fork x <- Z -> y and z is in Z
the path p with a set of nodes x, z, y contains a chain x -> Z <- y and z is NOT in Z



We compute it for the exposure variables. 
We can also verify these paths against the data.

"
paths(graph,from=exposures(graph),to=outcomes(graph),directed = TRUE)
#{EMPTY}
"If we considered all exposures (i.e., set their values), all directed paths are empty.
This means that only setting the value of this variables we cannot compute 
the causal effect on the outcome variable. We need to set the value of other variables.

"

paths(graph,from=exposures(graph),to=outcomes(graph),directed = FALSE)


# [1] "Programmer.Skill -> Confidence -> Accuracy"                                         
# [2] "Programmer.Skill -> Difficulty -> Duration -> Confidence -> Accuracy"               
# [3] "Programmer.Skill -> Difficulty -> Explanation -> Accuracy"                          
# [4] "Programmer.Skill -> Difficulty -> Explanation -> Duration -> Confidence -> Accuracy"

paths(graph,c("Programmer.Skill"),"Accuracy",directed = FALSE)
# [1] "Programmer.Skill -> Confidence -> Accuracy"                                                      
# [2] "Programmer.Skill -> Confidence <- Duration <- Difficulty -> Explanation -> Accuracy"             
# [3] "Programmer.Skill -> Confidence <- Duration <- Difficulty <- Task.Type -> Explanation -> Accuracy"
# [4] "Programmer.Skill -> Confidence <- Duration <- Explanation -> Accuracy"                           
# [5] "Programmer.Skill -> Difficulty -> Duration -> Confidence -> Accuracy"                            
# [6] "Programmer.Skill -> Difficulty -> Duration <- Explanation -> Accuracy"                           
# [7] "Programmer.Skill -> Difficulty -> Explanation -> Accuracy"                                       
# [8] "Programmer.Skill -> Difficulty -> Explanation -> Duration -> Confidence -> Accuracy"             
# [9] "Programmer.Skill -> Difficulty <- Task.Type -> Explanation -> Accuracy"                          
# [10] "Programmer.Skill -> Difficulty <- Task.Type -> Explanation -> Duration -> Confidence -> Accuracy"
# $open [1]  TRUE FALSE FALSE FALSE  TRUE FALSE  TRUE  TRUE FALSE FALSE

adjustmentSets(graph,exposure = "Task.Type",outcome = "Accuracy",effect = c("direct"))
#{ Confidence, ExplanationSize }
#{ Duration, ExplanationSize }
#{ Difficulty }
