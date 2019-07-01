install.packages("ggm")
install.packages("ggdag")

library(ggdag)

library( dagitty )
library (ggm)


g1 <- dagitty(
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

tidy_dagitty(g1)
ggdag(g1, layout = "circle")

impliedConditionalIndependencies(g1)
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



adjustmentSets(g,exposure = "Skill",outcome = "Accuracy",effect = c("direct"))
adjustmentSets(g,exposure = c("Skill","Complexity"),outcome = "Accuracy",effect = c("direct"))
