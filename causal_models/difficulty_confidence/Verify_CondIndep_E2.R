
"
Verify the Conditional Independencies implied by a Causal Graph (see below) 
Data in the context of Experiment-2.

This involves fitting regression models.

RESULTS OF VERIFYING THE FOLLOWING CLAIMS:

Claim 1.1: Code.Complexity _||_ Programmer.Score
#Not significant. tau 0.006988912 , p-value=0.6741
#Could not detect dependency between Qualification Score and Code Complexity (in LOCS)


Claim 1.2: Accuracy _||_ Duration | Code.Complexity, Confidence, Difficulty, Explanation, Programmer.Score
Consequence: Accuracy is not affected directly by Duration.
Confirmed no path between Duration and Accuracy. 
Test generalized for Developers, and Non-Students, but it did not generalize for Students

Claim 1.3: Confidence _||_ Explanation | Difficulty, Duration
Confirmed, Explanation coefficient crosses zero  
Test generalized Non-students and Students, but did not for Professionals Programmers, 
for whom explanation and confidence are not independent given Difficulty and Duration

# Claim 1.4: Duration _||_ Programmer.Score | Difficulty
Confirmed for Students, Non-Students.
Not confirmed for All and professional developers.

# Claim 1.5: Explanation _||_ Programmer.Score | Difficulty
Not confirmed 

#Claim 1.6: Code.Complexity _||_ Explanation | Difficulty
Confirmed and Generalized across all professions

#Claim 1.7: Code.Complexity _||_ Duration | Difficulty
Confirmed and Generalized across all professions


"


#Load data
"Load data with treatment field (isBugCovering) and ground truth (answer correct)"
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//load_consent_create_indexes_E2.R")


#------------------
library(rethinking)
library(stringr)
library(dplyr)

"Build regression models where 
Accuracy <- Confidence + Explanation + Code.Complexity + Duration + Programmer.Skill + Difficulty + Task.Type + Answer.Type

Assumptions: Difficulty and Confidence are considered continuous

Build one model for each of the four pairs of Answer.Type, Task.Type.

Label meannings
MB for Markov Blanket, ACC for accuracy, Bug for Task with Bug, Pos for Positive Answer (Yes's)
NBug for Task with No Bug, Neg for Negative Answer (No's or IDK's)

MB.ACC.Bug.Pos (accuracy of true positives)
MB.ACC.NBug.Neg (accuracy of true negatives)
MB.ACC.Bug.Pos (accuracy of false positives)
MB.ACC.NBug.Neg (accuracy of false negatives)


"

#MB.ACC.Bug.Pos (accuracy of true positives)
#Filter by bug and yes answers.
df <- df_E2

df <- select(df,"difficulty","confidence", "duration_minutes", 
             "explanation.size", "qualification_score",
              "isBugCovering_id","isAnswerCorrect_bol","profession",
             "LOC_original","LOC_inspection")

dim(df[df$duration_minutes>=60,])
#[1] 27  7
#Remove these as outliers
df <-  df[df$duration_minutes<60,]

# df <- df[complete.cases(df), ]
# dim(df)


#centering and normalizing
df$duration_minutes <- scale(df$duration_minutes)
df$explanation.size <- scale(df$explanation.size)
df$qualification_score <- scale(df$qualification_score)

#NOT CONVERGING!
# MB.ACC.Bug.Pos <- quap(
#   alist(
#     isAnswerCorrect_bol ~ dbinom(1,p),
#     logit(p) <- a + bdif[isBugCovering_id]*difficulty + bcon*confidence +
#       bdur*duration_minutes + bexp*explanation.size + bscore*qualification_score,
#     bdif[isBugCovering_id] ~ dbeta( 1 , 1 ) ,
#     bcon ~ dnorm( 0 , 1 ) ,
#     bdur ~ dnorm( 0 , 1 ) ,
#     bexp ~ dnorm( 0 , 1 ) ,
#     bscore ~ dnorm( 0 , 1 ) ,
#     a[isBugCovering_id] ~ dbeta(1, 1),
#     sigma ~ dbeta(1,1)
#   ), data = df
# ) 


"VERIFYING EACH OF CONDITIONAL INDEPENDENCIES CLAIMS"

#Claim 1.1:
cor.test(df_E2$qualification_score,df_E2$LOC_original, method="kendall", use="two.sided") 
#Not significant. tau 0.006988912 , p-value=0.6741
#Could not detect dependency between Qualification Score and Code Complexity (in LOCS)
#TODO: use Halstead Metrics that were already calculated
#TODO: apply qualification score from IRT Model


#Claim 1.2:
#Accuracy _||_ Duration | Code.Complexity, Confidence, Difficulty, Explanation, Programmer.Score
#Consequence: Accuracy is not affected directly by Duration.

claim1.2.All <- glm(isAnswerCorrect_bol ~ difficulty + confidence + duration_minutes 
                + explanation.size + qualification_score,
             family=binomial(link='logit'),data=df)
summary(claim1.2.All)
#Results
# Coefficients:
#                     Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)         -2.42224    0.07478 -32.393  < 2e-16 ***
#   difficulty           0.20196    0.08310   2.430   0.0151 *  
#   confidence           0.40654    0.09479   4.289 1.79e-05 ***
#   duration_minutes     0.08694    0.06559   1.326   0.1850    
#   explanation.size     0.09906    0.06198   1.598   0.1100    
#   qualification_score  0.14233    0.07385   1.927   0.0540 .  


#Duration, explanation.size, and qualification_score non-significant
#TODO apply qualification score from IRT Model

claim1.2.Professionals <- glm(isAnswerCorrect_bol ~ difficulty + confidence + duration_minutes 
                              + explanation.size + qualification_score,
                              family=binomial(link='logit'),
                              data=df[df$profession=="Professional_Developer" | df$profession=="Hobbyist" ,])
summary(claim1.2.Professionals)
#Only difficulty and confidence are significant, 
#difficulty (coef=0.19662, pvalue = 0.0142) and confidence (coef=0.22676, pvalue = 0.0142)


claim1.2.Non_Students <- glm(isAnswerCorrect_bol ~ difficulty + confidence + duration_minutes 
                              + explanation.size + qualification_score,
                          family=binomial(link='logit'),
                          data=df[df$profession=="Professional_Developer"
                                  | df$profession=="Hobbyist"
                                  | df$profession=="Other",])
summary(claim1.2.Non_Students )
#Only difficulty and confidence are significant, 
#difficulty (coef=0.24143, p-value = 0.012470) and confidence (coef=0.11087, pvalue = 0.000153)

claim1.2.Students <- glm(isAnswerCorrect_bol ~ difficulty + confidence + duration_minutes 
                              + explanation.size + qualification_score,
                              family=binomial(link='logit'),
                              data=df[df$profession=="Graduate_Student" | df$profession=="Undergraduate_Student" ,])
summary(claim1.2.Students)
#no coefficient is significant

#------------------------------------------------
#Claim 1.3

# Claim 1.3:
# Confidence _||_ Explanation | Difficulty, Duration
#Consequence: No path between Confidence and Explanation
claim1.3 <- function(dataframe){
  quap( alist(
    confidence ~ dnorm( mu , sigma ) ,
    mu <- a + bexp*explanation.size + bdur*duration_minutes + bdif*difficulty,
    bexp ~ dnorm( 0 , 1 ) ,
    bdur ~ dnorm( 0 , 1 ) ,
    bdif ~ dnorm( 0 , 1 ) ,
    a ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = dataframe
) 
}

precis(claim1.3(df))
#CONFIRMED. Explanation coefficient crosses zero, hence 
#confidence is independent of explanation size conditioned 
#on difficulty and duration
#       mean   sd  5.5% 94.5%
# bexp  -0.02 0.03 -0.06  0.03 CROSSES ZERO
# bdur   0.06 0.03  0.01  0.10
# bdif  -0.70 0.02 -0.74 -0.67
# a      5.53 0.07  5.42  5.64
# sigma  1.35 0.02  1.32  1.38

precis(claim1.3(dataframe=df[df$profession=="Graduate_Student" 
                             | df$profession=="Undergraduate_Student" ,]))
#CONFIRMED. Explanation coefficient crosses zero but also Duration

precis(claim1.3(dataframe=df[df$profession=="Professional_Developer"
                                  | df$profession=="Hobbyist"
                                  | df$profession=="Other",]))
#CONFIRMED. Only Explanation coefficient crosses zero

precis(claim1.3(dataframe=df[df$profession=="Professional_Developer",]))
#NOT CONFIRMED. Only Duration crosses Zero


#-------------------------------------

# Claim 1.4
# Duration _||_ Programmer.Score | Difficulty

claim1.4 <- function(dataframe){
  quap( alist(
    duration_minutes ~ dnorm( mu , sigma ) ,
    mu <- a + bscore*qualification_score + bdif*difficulty,
    bscore ~ dnorm( 0 , 1 ) ,
    bdif ~ dnorm( 0 , 1 ) ,
    a ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = dataframe
  ) 
}

precis(claim1.4(df))
#         mean   sd  5.5% 94.5%
# bscore  0.06 0.02  0.03  0.10
# bdif    0.11 0.02  0.08  0.13
# a      -0.32 0.05 -0.40 -0.24
# sigma   0.99 0.01  0.97  1.01
#NOT CONFIRMED
precis(claim1.4(dataframe=df[df$profession=="Graduate_Student" 
                             | df$profession=="Undergraduate_Student" ,]))
#CONFIRMED. Score coefficient crosses zero

precis(claim1.4(dataframe=df[df$profession=="Professional_Developer"
                             | df$profession=="Hobbyist"
                             | df$profession=="Other",]))
#NOT CONFIRMED.

precis(claim1.4(dataframe=df[df$profession=="Professional_Developer",]))
#CONFIRMED. Score coefficient crosses zero


#-----------------------------------------------
# Claim 1.5
# Explanation _||_ Programmer.Score | Difficulty
claim1.5 <- function(dataframe){
  quap( alist(
    explanation.size ~ dnorm( mu , sigma ) ,
    mu <- a + bscore*qualification_score + bdif*difficulty,
    bscore ~ dnorm( 0 , 1 ) ,
    bdif ~ dnorm( 0 , 1 ) ,
    a ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = dataframe
  ) 
}

precis(claim1.5(df))
#         mean   sd  5.5% 94.5%
# bscore  0.07 0.02  0.04  0.10
# bdif   -0.04 0.02 -0.06 -0.01
# a       0.11 0.05  0.03  0.19
# sigma   1.00 0.01  0.97  1.02
#NOT CONFIRMED
precis(claim1.5(dataframe=df[df$profession=="Graduate_Student" 
                             | df$profession=="Undergraduate_Student" ,]))
#NOT CONFIRMED

precis(claim1.5(dataframe=df[df$profession=="Professional_Developer"
                             | df$profession=="Hobbyist"
                             | df$profession=="Other",]))
#NOT CONFIRMED, only difficulty crosses zero

precis(claim1.5(dataframe=df[df$profession=="Professional_Developer",]))
#CONFIRMED. Both Score and Difficulty coefficient cross zero

#Same happened with a frequentist model.
model <- lm(explanation.size ~ qualification_score + difficulty, data=df)
summary(model) #p-values < 0.05

#-------------------------------------------------

# Claim 1.6 
# Code.Complexity _||_ Explanation | Difficulty

claim1.6 <- function(dataframe){
  quap( alist(
    explanation.size ~ dnorm( mu , sigma ) ,
    mu <- a + bloc*LOC_inspection + bdif*difficulty,
    bloc ~ dnorm( 0 , 1 ) ,
    bdif ~ dnorm( 0 , 1 ) ,
    a ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = dataframe
  ) 
}

precis(claim1.6(df))
#         mean   sd  5.5% 94.5%
# bloc   0.01 0.01  0.00  0.03
# bdif  -0.04 0.02 -0.07 -0.02
# a      0.09 0.05  0.00  0.18
# sigma  1.00 0.01  0.98  1.02
#CONFIRMED coefficient for LOC_inspection crosses zero

precis(claim1.6(dataframe=df[df$profession=="Graduate_Student" 
                             | df$profession=="Undergraduate_Student" ,]))
#CONFIRMED coefficient for LOC_inspection crosses zero

precis(claim1.6(dataframe=df[df$profession=="Professional_Developer"
                             | df$profession=="Hobbyist"
                             | df$profession=="Other",]))
#CONFIRMED coefficient for LOC_inspection crosses zero

precis(claim1.6(dataframe=df[df$profession=="Professional_Developer",]))
#CONFIRMED coefficient for LOC_inspection crosses zero

model <- lm(explanation.size ~ LOC_inspection + difficulty, data=df)
summary(model) #coefficient for LOC_inspection p-value=0.10031 
#CONFIRMED 

#-------------------------------------------------

# Claim 1.7
# Code.Complexity _||_ Duration | Difficulty

claim1.7 <- function(dataframe){
  quap( alist(
    duration_minutes ~ dnorm( mu , sigma ) ,
    mu <- a + bloc*LOC_inspection + bdif*difficulty,
    bloc ~ dnorm( 0 , 1 ) ,
    bdif ~ dnorm( 0 , 1 ) ,
    a ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = dataframe
  ) 
}

precis(claim1.7(df))
#         mean   sd  5.5% 94.5%
# bloc   0.01 0.01  0.00  0.03
# bdif  -0.04 0.02 -0.07 -0.02
# a      0.09 0.05  0.00  0.18
# sigma  1.00 0.01  0.98  1.02
#CONFIRMED coefficient for LOC_inspection crosses zero

precis(claim1.7(dataframe=df[df$profession=="Graduate_Student" 
                             | df$profession=="Undergraduate_Student" ,]))
#CONFIRMED coefficient for LOC_inspection crosses zero

precis(claim1.7(dataframe=df[df$profession=="Professional_Developer"
                             | df$profession=="Hobbyist"
                             | df$profession=="Other",]))
#CONFIRMED coefficient for LOC_inspection crosses zero

precis(claim1.7(dataframe=df[df$profession=="Professional_Developer",]))
#CONFIRMED coefficient for LOC_inspection crosses zero

model <- lm(duration_minutes ~ LOC_inspection + difficulty, data=df)
summary(model) #coefficient for LOC_inspection p-value=0.232 
#CONFIRMED 






