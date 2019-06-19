
library( dagitty )

g <- dagitty(
  "dag{
  TaskType -> Difficulty;
  Skill -> Difficulty;
  Complexity -> Difficulty;
  Difficulty -> Duration;
  ExplanationSize -> Duration;
  Duration -> Confidence;
  Confidence -> Accuracy
  ExplanationSize -> Accuracy
  TaskType [exposure]
  Skill [exogenous]
  Complexity [exogenous]
  Difficulty [endogenous]
  ExplanationSize [endogenous]
  Duration [endogenous]
  Confidence [endogenous]
  Accuracy [outcome]
  }"
)

g$$paths(g,"Skill","Difficulty")

paths(g,"TaskType","Accuracy")

adjustmentSets(g,exposure = "TaskType",outcome = "Accuracy",effect = c("direct"))
{ Confidence, ExplanationSize }
{ Duration, ExplanationSize }
{ Difficulty }
