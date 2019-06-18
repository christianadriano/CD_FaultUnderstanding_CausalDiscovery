
library( dagitty )

g <- dagitty( "dag{
              Skill -> Difficulty;
              Complexity -> Difficulty;
              Difficulty -> Duration;
              ExplanationSize -> Duration;
              Difficulty -> Confidence;
              Confidence -> Accuracy
              ExplanationSize -> Accuracy
              }" )

paths(g,"Skill","Accuracy")

