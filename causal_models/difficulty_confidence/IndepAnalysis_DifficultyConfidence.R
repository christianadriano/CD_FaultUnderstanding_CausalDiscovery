"
Conditional independence Analysis for Difficulty and Confidence of tasks

Building
Specify a causal graph based on assumptions or extant theory
Generate Propositions from implied conditional independencies

Further studies in other files:

Verification
Verify marginal independences (unconditional)
Verify conditional independencies
Verify Markov Blankets for Difficulty and Confidence

Validation
Compute Paths (direct and indirect) between Difficulty and Accuracy
Generate adjustments necessary to compute effects through these paths given a conditioning set


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

#Equivalent DAGs = 2
length(equivalentDAGs( graph))
eq_dags <- equivalentDAGs(graph)

dag_eqv_2 <- eq_dags[2]
dag_eqv_2 <- gsub("\n"," ",dag_eqv_2)
graph_eqv_2 <- dagitty(dag_eqv_2)
plot(graph_eqv_2)

#The only difference is that the reversal of the arrow from 
#Duration to Explanation. This will be relevant when we model

#This means that the implied conditional independencies can be produced by two 
#different graphs.

impliedConditionalIndependencies(graph)

"INDEPENDENCIES IMPLIED BY THE CAUSAL GRAPH

After applying the d-separation criterion to all pairs of variables, we
obtained a set of independencies that are implied by the causal graph model.

1 Marginal independencies
Some marginal independencies exist by definition, because the covariates are exposure
variables that were randomized in the experiment design.

1.1 Expected marginal independencies from the randomized task allocation

Claim 1.1: 
Code.Complexity _||_ Programmer.Score


1.2 Conditional Independencies 

Related to the Outcome Variable

Claim 1.2:
Accuracy _||_ Duration | Code.Complexity, Confidence, Difficulty, Explanation, Programmer.Score
Consequence: Accuracy is not affected directly by Duration.

Dependent variables related

Claim 1.3:
Confidence _||_ Explanation | Difficulty, Duration
Consequence: No path between Confidence and Explanation

Claim 1.4
Duration _||_ Programmer.Score | Difficulty
Consequence: Programmer score does not affect Duration directly.
It only affects duration by affecting Difficulty

Claim 1.5
Explanation _||_ Programmer.Score | Difficulty
Consequence: Programmers score does not affect the size of
Explanations. It does so only indirectly through Difficulty

Claim 1.6 
Code.Complexity _||_ Explanation | Difficulty

Claim 1.7
Code.Complexity _||_ Duration | Difficulty

Consequence: These indepependencies of Code.Complexity claim that 
there are not paths between Code.Complexity and Duration or Explanation

VERIFYING THE MODEL

How many regression equations (tests) are needed to ensure that the model is fully verified? 
i.e., if the model passes all tests, then we can be certain that the model
cannot be refuted by additional tests of these kind. A test consists of
fitting a regression model where one of the conditionally independent variable
is explained by both the other conditionally independent variable and 
the variables that are conditioned on. To pass the test, zero must be in the
credible interval of the coefficient of the independent variable.

To discover the minimum set of tests, we just need the basis set
"
impliedConditionalIndependencies( graph, type="basis.set" )
"
Accuracy _||_ Duration | Code.Complexity, Confidence, Difficulty, Explanation, Programmer.Score
Code.Complexity _||_ Programmer.Score
Confidence _||_ Explanation | Code.Complexity, Difficulty, Duration, Programmer.Score
Duration _||_ Code.Complexity, Programmer.Score | Difficulty, Explanation
Explanation _||_ Code.Complexity, Programmer.Score | Difficulty

We need 5 regressions. We will compute these in the following file

Verify_CondIndep_E2.R

"

