
install.packages("ggm")
install.packages("ggdag")

library(ggdag)

library( dagitty )
library (ggm)

# g <- dagitty(
#   "dag{
#   TaskType -> Difficulty;
#   Skill -> Difficulty;
#   Complexity -> Difficulty;
#   Difficulty -> Duration;
#   ExplanationSize -> Duration;
#   Duration -> Confidence;
#   Confidence -> Accuracy
#   ExplanationSize -> Accuracy
#   TaskType [exposure]
#   Skill [exogenous]
#   Complexity [exogenous]
#   Difficulty [endogenous]
#   ExplanationSize [endogenous]
#   Duration [endogenous]
#   Confidence [endogenous]
#   Accuracy [outcome]
#   }"
# )

d <- 'a means "casa" '

dag <- ' dag {
bb="-0.5,-0.5,0.5,0.5";
Accuracy [outcome,pos="-0.018,0.091"];
AnswerType [pos="-0.254,0.002"];
CodeComplexity [exposure, pos="-0.324,-0.233"];
Confidence [pos="-0.115,-0.034"];
Duration [pos="-0.037,-0.095"];
ExplanationSize [pos="0.061,-0.176"];
PerceivedDifficulty [pos="-0.142,-0.179"];
ProgrammerSkill [exposure,pos="-0.327,-0.120"];
TaskType [exposure,pos="-0.044,-0.318"];
AnswerType -> Confidence;
CodeComplexity -> PerceivedDifficulty;
Confidence -> Accuracy;
Duration -> Confidence;
ExplanationSize -> Accuracy;
ExplanationSize -> Duration;
PerceivedDifficulty -> Duration;
PerceivedDifficulty -> ExplanationSize;
ProgrammerSkill -> Confidence;
ProgrammerSkill -> PerceivedDifficulty;
TaskType -> ExplanationSize;
TaskType -> PerceivedDifficulty;
}'
graph <- dagitty(dag)

tidy_dagitty(graph)
ggdag(graph, layout = "circle")
#check layout and color options:
#https://cran.r-project.org/web/packages/ggdag/vignettes/intro-to-ggdag.html



p <- paths(graph,c("ProgrammerSkill"),"Accuracy",directed = TRUE)
#$paths
#[1] "TaskType -> Difficulty -> Duration -> Confidence -> Accuracy"     
#[2] "TaskType -> Difficulty -> Duration <- ExplanationSize -> Accuracy"
#$open TRUE FALSE


adjustmentSets(graph,exposure = "TaskType",outcome = "Accuracy",effect = c("direct"))
#{ Confidence, ExplanationSize }
#{ Duration, ExplanationSize }
#{ Difficulty }
