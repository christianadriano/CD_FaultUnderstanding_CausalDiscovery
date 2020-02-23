"
Causal effects of Difficulty on Accuracy.

To compute the causal effect we need to identify the backdoor paths that 
can bias the cause-effect of Difficulty on Accuracy. For that we apply
the backdoor criterion [Peals 2009], which consists of finding a set 
of variables Z that blocks every path between the exposure variable X
and the outcome variable Y and that contains an arrow into X.

If the set of variables Z satisfies the backdoor criterion for X and Y, then 
we can compute the causal effect of X on Y by the following formula:

E(Y = y | do(X=x)) = SUM_over_Z{ P(Y=y|X=x,Z=z)P(Z=z) }

The Z set is also called adjustment set.

The correct Z set blocks the backdoor paths and also satisfies the following
conditions:
- It blocks alls spurious paths between X and Y (by definition)
- It leaves all directed paths from X to Y open
- It does not create anhy new spurious paths. Spurious paths are created when we
condition on a collider node.

Next we will look at the alternatives of adjustments sets to compute the 
effect of Difficulty on Accuracy.

References: http://dagitty.net/primer/

"
#Load the graph
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//causal_models//difficulty_confidence//DAG_Tasks_E2.R")

#result is on the variable graph

"The graph is set with the following exposure variables:"
exposures(graph) #"Code.Complexity"  "Programmer.Score
outcomes(graph) #"Accuracy"

"However, I want investigate the effect of a exposure to a certain Difficulty level, hence 
need to change the exposures from the default ones."
exposures(graph) <- c() #remove exposure info
exposures(graph) <- c("Difficulty") #remove exposure info

adjustmentSets(graph,exposure = "Difficulty", outcome="Accuracy", type="minimal", effect = "total")
"For total effect, I would need to adjust for { Code.Complexity, Programmer.Score }.
The total effect considers all non-spurious paths between Difficulty and Accuracy

However, if I want to include in my formula only the direct effect of difficulty, then I would need
a larger adjustment set. { Code.Complexity, Confidence, Explanation, Programmer.Score }. This means 
that I would need to include Confidence and Explanation.
"

adjustmentSets(graph,exposure = "Difficulty", outcome="Accuracy", type="minimal", effect = "direct")

"CONDITIONAL INTERVENTIONS AND COVARIATE-SPECIFIC EFFECTS

I am also interested in kwowing the total effect of Difficulty D for the specific value of Confidence C,
for instance, high confidence.

For that we need to average the conditional probabilities 

E(Accuracy | do(D=5), C=5 ) = SUM_over_Z{ P(Y=y|D=5,Z=z,C=5)P(Z=z|C=5) }
5 means high confidence and 1 means low confidence.

So, basically, what we are doing is to condition the distribution probability of the 
adjustment set Z to the Confidence-specific effect P(Z=z|C=5). We use this condition probability 
to obtain the weighted average of the joint probability distribution  P(Y=y|D=5,Z=z,C=5). 

Next we need to fit a regression model for these interventions. 
This will be done in a different script.


"
adjustedNodes(graph)
adjustmentSets(graph,exposure = "Difficulty", outcome="Accuracy", type="all", effect = "total")
adjustmentSets(graph,exposure = "Difficulty", outcome="Accuracy", type="minimal", effect = "direct")

adjustedNodes(graph) <- c("Confidence")
adjustmentSets(graph,exposure = "Difficulty", outcome="Accuracy", type="all", effect = "total")
adjustmentSets(graph,exposure = "Difficulty", outcome="Accuracy", type="minimal", effect = "direct")

#There is no effect on the adjustment sets by adding confidence as an adjustedNode.



