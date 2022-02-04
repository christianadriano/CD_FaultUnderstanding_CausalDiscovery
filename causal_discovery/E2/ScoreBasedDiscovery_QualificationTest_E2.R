
"
Score-Based Causal discovery for the programming skill and test Duration related factors

profession [exogenous];
years_programming [exogenous];
age [exogenous]
test_duration [endogenous];
qualification_score [outcome];
adjusted_score [outcome];

" 
library(bnlearn)
library(dplyr)

#Load only Consent data. No data from tasks, only from demographics and qualification test
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E2.R")
df_consent <- load_consent_create_indexes()

#Evaluate how fast and slow can explain adjusted_score score
df_consent_fast <- df_consent[df_consent$is_fast,]
df_consent_slow <- df_consent[!df_consent$is_fast,]

df_consent <- rename(df_consent,speed=testDuration_fastMembership)
df_consent <- rename(df_consent,years_prog=years_programming)

df_selected <-
  dplyr::select(df_consent,
                #profession, #not using because it is categorical and the current methods do not support it
                age,
                years_prog,
                test_duration,
                speed, #or use is_fast, which is binary
                adjusted_score #outcome
  );

#df_selected$profession <- as.factor(df_selected$profession)

node.names <- colnames(df_selected)

#years_prog is not parent of age.
blacklist_1 <- data.frame(from = c("years_prog"), 
                          to   = c("age"))

#Prevent speed to be parents of age and years_prog and profession
blacklist_2 <- data.frame(from = c("speed"), 
                          to   = c("years_prog","age"))#,"profession"))

#profession has parent nodes
#blacklist_3 <- data.frame(from = node.names[-grep("profession", node.names)], 
#                          to   = c("profession"))
#test_duration is not parent of age, years_prog, profession
blacklist_4 <- data.frame(from = c("test_duration"),
                          to   = c("years_prog","age"))#"profession","is_fast"))
#adjusted_score cannot be parent of anyone
blacklist_5 <- data.frame(from = c("adjusted_score"),
                          to   = node.names[-grep("adjusted_score", node.names)])

#Task Accuracy can only be measured with all tasks data. 
#Here we are dealing only with programmer demographic data.

blacklist_all <- rbind(blacklist_1,blacklist_2,blacklist_4,blacklist_5) 
#------------------------------------------
#Including Profession

bn <- pc.stable(df_selected,blacklist = blacklist_all)
plot(bn,main="All Professions, Constraint-Based Discovery")

bn <-tabu(df_selected,blacklist = blacklist_all)
plot(bn,main="All Professions, Score-Based Discovery")

#-----------------------------------------
#BY PROFESSION

#Remove profession from blacklist
blacklist_all <- blacklist_all[!(blacklist_all$from %in% c("profession") ),]
blacklist_all <- blacklist_all[!(blacklist_all$to %in% c("profession") ),]
#blacklist_all <- blacklist_all[!(blacklist_all$from %in% c("speed") ),]
#blacklist_all <- blacklist_all[!(blacklist_all$to %in% c("speed") ),]


#Run structure discovery for each profession
professions = c("Other", "Undergraduate_Student","Graduate_Student","Hobbyist",
                "Programmer","Professional")

#Constraint-Based Algorithm
for (i in 1:length(professions)) {
  choice = professions[i]
  df_prof <- df_selected[df_selected$profession==choice,]
  df_prof <- 
    dplyr::select(df_prof,
                  years_prog,
                  age,
                  speed,
                  test_duration,
                  adjusted_score
    );
  bn <-pc.stable(df_prof,blacklist = blacklist_all)
  plot(bn,main=choice)
  #graphviz.plot(bn,main=choice,shape="ellipse",layout = "circo");
}

"Analysis of results of the PC algorithm
test duration seem relevant only for professional, undergrad, grad, hobbyist
only in undegrad that test duration is affected by years_prog"

#Score-based algorithm - Hill Climbing
for (i in 1:length(professions)) {
  choice = professions[i]
  df_prof <- df_selected[df_selected$profession==choice,]
  df_prof <- 
    dplyr::select(df_prof,
                  years_prog,
                  age,
                  speed,
                  test_duration,
                  adjusted_score
    );
  bn <-tabu(df_prof,blacklist = blacklist_all)
  plot(bn,main=choice)
  #graphviz.plot(bn,main=choice,shape="ellipse",layout = "circo");
}

"Analysis of results of the Tabu algorithm
Test duration has not effect on adjusted_score for other and Programmer
Test duration has no parents for Graduate and Professional
Only in Hobbyists that test duration is a mediator for effect on adjusted_score
Test duration has years_prog as parent in Hobbyist, Undergrad, 
Programmer, and Other.
"

#-------------------------------------
#Using now the qualification_score

df_selected <-
  dplyr::select(df_consent,
                profession, #categorical
                age,
                years_prog,
                test_duration,
                speed, #or use is_fast, which is binary
                qualification_score #outcome
  );


node.names <- colnames(df_selected)

#years_prog is not parent of age.
blacklist_1 <- data.frame(from = c("years_prog"), 
                          to   = c("age"))
#profession has parent nodes
blacklist_2 <- data.frame(from = node.names[-grep("profession", node.names)], 
                          to   = c("profession"))
#test_duration is not parent of age, years_prog, profession
blacklist_3 <- data.frame(from = c("test_duration"),
                          to   = c("profession","years_prog","age"))
#qualification_score cannot be parent of anyone
blacklist_4 <- data.frame(from = c("qualification_score"),
                          to   = node.names[-grep("qualification_score", node.names)])
#Prevent speed to be parents of age and years_prog and profession
blacklist_5 <- data.frame(from = c("speed"), 
                          to   = c("years_prog","age","profession"))


blacklist_all <- rbind(blacklist_1,blacklist_2,blacklist_3,blacklist_4,blacklist_5) 

#------------------------------------------
#Including Profession

bn <- pc.stable(df_selected,blacklist = blacklist_all)
plot(bn,main="All Professions, Constraint-Based Discovery")

bn <-tabu(df_selected,blacklist = blacklist_all)
plot(bn,main="All Professions, Score-Based Discovery")


#Remove profession from blacklist
blacklist_all <- blacklist_all[!(blacklist_all$from %in% c("profession") ),]
blacklist_all <- blacklist_all[!(blacklist_all$to %in% c("profession") ),]

#Constraint-Based Algorithm
for (i in 1:length(professions)) {
  choice = professions[i]
  df_prof <- df_selected[df_selected$profession==choice,]
  df_prof <- 
    dplyr::select(df_prof,
                  years_prog,
                  age,
                  speed,
                  test_duration,
                  adjusted_score
    );
  bn <-pc.stable(df_prof,blacklist = blacklist_all)
  plot(bn,main=choice)
  #graphviz.plot(bn,main=choice,shape="ellipse",layout = "circo");
}

"Analysis of results of the PC algorithm
Test duration has not effect on adjusted_score for other and Programmer
Only in undegrad that test duration is affected by years_prog
Hence, qualification_Score and adjusted_score produced the same graphs with the PC algorithm
"

#Score-based algorithm - Hill Climbing
for (i in 1:length(professions)) {
  choice = professions[i]
  df_prof <- df_selected[df_selected$profession==choice,]
  df_prof <- 
    dplyr::select(df_prof,
                  years_prog,
                  age,
                  speed,
                  test_duration,
                  adjusted_score
    );
  bn <-tabu(df_prof,blacklist = blacklist_all)
  plot(bn,main=choice)
  #graphviz.plot(bn,main=choice,shape="ellipse",layout = "circo");
}

"Analysis of results of the Tabu algorithm
Test duration has not effect on adjusted_score for other and Programmer
Test duration has no parents for Graduate and Professional
Only in Hobbyists that test duration is a mediator for effect on adjusted_score
Test duration has years_prog as parent in Hobbyist, Undergrad, 
Programmer, and Other.

Hence, qualification_score and adjusted_score produced the same graph with the tabu algorithm
"


#TODO
#compare how professions are distinct in terms of adjusted_score and qualification_score
#Compare the strength of graph connections using adjusted_score and qualification_score

#https://arxiv.org/pdf/0908.3817.pdf

