"
Generate and Plot DAG for the E2 bug inspection tasks.

This script is used by the following types of analyzes:
- Identify Conditional Independencies (e.g., IndepAnalysis_DifficultyConfidence.R)
- Verify Condition Independencies (e.g., Verify_CondIndep_E2)

- Identify Markov Blanket (e.g., MrkvBlnkt_Accuracy_E2.R)
- Verify Markov Blanket (e.g., Verify_CondIndep_E2)

- Identify Backdoor paths (e.g., BackdoorCrit_DifficultyAccuracy_E2.R)
- Verify Backdoor paths (e.g., Verify_Backdoors_Difficulty-Accuracy_E2)
- Compute causal effects 

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

