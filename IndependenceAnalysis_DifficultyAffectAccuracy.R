install.packages("ggm")
install.packages("ggdag")

library(ggdag)

library( dagitty )
library (ggm)

g_0 <- dagitty(
  "dag{
  Skill -> Difficulty;
  Complexity -> Difficulty;
  Difficulty -> Accuracy;
  Skill [exogenous]
  Complexity [exogenous]
  Difficulty [endogenous]
  Accuracy [outcome]
  }"
)

tidy_dagitty(g_0)
ggdag(g_0, layout = "circle")

impliedConditionalIndependencies(g_0)
# Accuracy _||_ Complexity | Difficulty
# Accuracy _||_ Skill | Difficulty
# Complexity _||_ Skill

" Model includes direct effect from Skill to Accuracy"
g_SA <- dagitty(
  "dag{
  Skill -> Difficulty;
  Complexity -> Difficulty;
  Skill -> Accuracy;
  Difficulty -> Accuracy;
  Skill [exogenous]
  Complexity [exogenous]
  Difficulty [endogenous]
  Accuracy [outcome]
  }"
)

tidy_dagitty(g_SA)
ggdag(g_SA, layout = "circle")

impliedConditionalIndependencies(g_SA)
# Accuracy _||_ Complexity | Difficulty, Skill
# Complexity _||_ Skill

" Model includes direct effect from Code Complexity to Accuracy"
g_CA <- dagitty(
  "dag{
  Skill -> Difficulty;
  Complexity -> Difficulty;
  Complexity -> Accuracy;
  Difficulty -> Accuracy;
  Skill [exogenous]
  Complexity [exogenous]
  Difficulty [endogenous]
  Accuracy [outcome]
  }"
)

tidy_dagitty(g_CA)
ggdag(g_CA, layout = "circle")

impliedConditionalIndependencies(g_CA)
# Accuracy _||_ Skill | Complexity, Difficulty
# Complexity _||_ Skill


" Model includes direct effects from Code Complexity and Skill to Accuracy"
g_CSA <- dagitty(
  "dag{
  Skill -> Difficulty;
  Complexity -> Difficulty;
  Skill -> Accuracy;
  Complexity -> Accuracy;
  Difficulty -> Accuracy;
  Skill [exogenous]
  Complexity [exogenous]
  Difficulty [endogenous]
  Accuracy [outcome]
  }"
)

tidy_dagitty(g_CSA)
ggdag(g_CSA, layout = "circle")

impliedConditionalIndependencies(g_CSA)
# Complexity _||_ Skill

p <- paths(g,c("Skill"),"Accuracy",directed = TRUE)
p

"Direct effect of Difficulty on Accuracy in each model"
adjustmentSets(g_0,exposure = "Difficulty",outcome = "Accuracy",effect = c("direct"))
#{}
adjustmentSets(g_CA,exposure = "Difficulty",outcome = "Accuracy",effect = c("direct"))
#{Complexity}
adjustmentSets(g_SA,exposure = "Difficulty",outcome = "Accuracy",effect = c("direct"))
#{Skill}
adjustmentSets(g_CSA,exposure = "Difficulty",outcome = "Accuracy",effect = c("direct"))
#{Skill,Complexity}

"Total effect of Difficulty on Accuracy in each model"
adjustmentSets(g_0,exposure = "Difficulty",outcome = "Accuracy",effect = c("total"))
#{}
adjustmentSets(g_CA,exposure = "Difficulty",outcome = "Accuracy",effect = c("total"))
#{Complexity}
adjustmentSets(g_SA,exposure = "Difficulty",outcome = "Accuracy",effect = c("total"))
#{Skill}
adjustmentSets(g_CSA,exposure = "Difficulty",outcome = "Accuracy",effect = c("total"))
#{Skill,Complexity}

"Direct effect of Skill on Accuracy in each model"
adjustmentSets(g_0,exposure = "Skill",outcome = "Accuracy",effect = c("direct"))
#{Difficulty}
adjustmentSets(g_CA,exposure = "Skill",outcome = "Accuracy",effect = c("direct"))
#{Difficulty,Complexity}
adjustmentSets(g_SA,exposure = "Skill",outcome = "Accuracy",effect = c("direct"))
#{Difficulty}
adjustmentSets(g_CSA,exposure = "Skill",outcome = "Accuracy",effect = c("direct"))
#{Difficulty,Complexity}

"Total effect of Skill on Accuracy in each model"
adjustmentSets(g_0,exposure = "Skill",outcome = "Accuracy",effect = c("total"))
#{}
adjustmentSets(g_CA,exposure = "Skill",outcome = "Accuracy",effect = c("total"))
#{}
adjustmentSets(g_SA,exposure = "Skill",outcome = "Accuracy",effect = c("total"))
#{}
adjustmentSets(g_CSA,exposure = "Skill",outcome = "Accuracy",effect = c("total"))
#{}


adjustmentSets(g_CA,exposure = c("Skill","Complexity"),outcome = "Accuracy",effect = c("direct"))

