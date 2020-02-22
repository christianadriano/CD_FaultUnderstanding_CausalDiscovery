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
