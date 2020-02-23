"
Causal effects of Difficulty on Accuracy.

To compute the causal effect we need to identify the backdoor paths that 
can bias the cause-effect of Difficulty on Accuracy. For that we apply
the backdoor criterion [Peals 2009], which consists of finding a set 
of variables Z that blocks every path between the exposure variable X
and the outcome variable Y and that contains an arrow into X.

If the set of variables Z satisfies the backdoor criterion for X and Y, then 
we can compute the causal effect of X on Y by the following formula:

P(Y = | do(X=x)) = SUM_over_Z{ P(Y=y|X=x,Z=z)P(Z=z) }

The Z set is also called adjustment set.

The correct Z set blocks the backdoor paths and also satisfies the following
conditions:
- It blocks alls spurious paths between X and Y (by definition)
- It leaves all directed paths from X to Y open
- It does not create anhy new spurious paths. Spurious paths are created when we
condition on a collider node.

Next we will look at the alternatives of adjustments sets to compute the 
effect of Difficulty on Accuracy.

"

