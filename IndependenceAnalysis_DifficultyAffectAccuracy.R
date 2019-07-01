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




p <- paths(g,c("Skill"),"Accuracy",directed = TRUE)
p



adjustmentSets(g,exposure = "Skill",outcome = "Accuracy",effect = c("direct"))
adjustmentSets(g,exposure = c("Skill","Complexity"),outcome = "Accuracy",effect = c("direct"))
