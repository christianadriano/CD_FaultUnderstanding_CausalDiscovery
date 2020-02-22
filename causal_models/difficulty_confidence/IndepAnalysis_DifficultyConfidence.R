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

Claim 1.6 and 1.7
Code.Complexity _||_ Duration | Difficulty
Code.Complexity _||_ Explanation | Difficulty
Consequence: These indepependencies of Code.Complexity claim that 
there are not paths between Code.Complexity and Duration or Explanation

"
