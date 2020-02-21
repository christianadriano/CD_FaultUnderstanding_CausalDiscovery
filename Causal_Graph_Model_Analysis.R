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

# g <- dagitty(
#   "dag{
#   Task -> Difficulty;
#   Skill -> Difficulty;
#   Complexity -> Difficulty;
#   Difficulty -> Duration;
#   ExplanationSize -> Duration;
#   Duration -> Confidence;
#   Confidence -> Accuracy
#   ExplanationSize -> Accuracy
#   Task [exposure]
#   Skill [exogenous]
#   Complexity [exogenous]
#   Difficulty [endogenous]
#   ExplanationSize [endogenous]
#   Duration [endogenous]
#   Confidence [endogenous]
#   Accuracy [outcome]
#   }"
# )


dag <- ' dag {
bb="-0.5,-0.5,0.5,0.5";
Accuracy [outcome,pos="-0.018,0.091"];
Answer [pos="-0.254,0.002"];
Code.Complexity [exposure, pos="-0.324,-0.233"];
Confidence [pos="-0.115,-0.034"];
Duration [pos="-0.037,-0.095"];
Explanation [pos="0.061,-0.176"];
Difficulty [pos="-0.142,-0.179"];
Programmer.Skill [exposure,pos="-0.327,-0.120"];
Task [exposure,pos="-0.044,-0.318"];
Answer -> Confidence;
Code.Complexity -> Difficulty;
Confidence -> Accuracy;
Duration -> Confidence;
Explanation -> Accuracy;
Explanation -> Duration;
Difficulty -> Duration;
Difficulty -> Explanation;
Programmer.Skill -> Confidence;
Programmer.Skill -> Difficulty;
Task -> Explanation;
Task -> Difficulty;
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


p <- paths(graph,c("Programmer.Skill"),"Accuracy",directed = TRUE)
#$paths
#[1] "Task -> Difficulty -> Duration -> Confidence -> Accuracy"     
#[2] "Task -> Difficulty -> Duration <- ExplanationSize -> Accuracy"
#$open TRUE FALSE



adjustmentSets(graph,exposure = "Task",outcome = "Accuracy",effect = c("direct"))
#{ Confidence, ExplanationSize }
#{ Duration, ExplanationSize }
#{ Difficulty }
