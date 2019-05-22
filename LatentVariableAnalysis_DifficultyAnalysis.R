" 
------------------------------------------------------------------

Computing moderation effect


http://lavaan.ugent.be/

------------------------------------------------------------------

"

install.packages("lavaan", dependencies = TRUE)
library(lavaan)

"Model"

model <- 'Skill <- YoE + Grade + Profession
          Difficulty <- Skill + Duration + ExplanationSize'

fit <- cfa(model, data=answers.data)

summary(fit,fit.measures=TRUE)

  