
"
Causal discovery for the programming skill and test Duration related factors

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

#Evaluate how fast and slow can explain adjusted_score score
df_consent_fast <- df_consent[df_consent$is_fast,]
df_consent_slow <- df_consent[!df_consent$is_fast,]

df_selected <-
  dplyr::select(df_consent_slow,
                profession, #categorical
                age,
                years_programming,
                test_duration,
                #is_fast, #binary
                adjusted_score #outcome
                );

df_selected$profession <- as.factor(df_selected$profession)
#df_selected$is_fast <- as.factor(df_selected$is_fast)

node.names <- colnames(df_selected)

#years_programming is not parent of age.
blacklist_1 <- data.frame(from = c("years_programming"), 
                          to   = c("age"))

#Prevent is_fast to be parents of age and years_programming and profession
#blacklist_2 <- data.frame(from = c("is_fast"), 
#                            to   = c("years_programming","age","profession"))

#profession has parent nodes
blacklist_3 <- data.frame(from = node.names[-grep("profession", node.names)], 
                          to   = c("profession"))
#test_duration is not parent of age, years_programming, profession
blacklist_4 <- data.frame(from = c("test_duration"),
                          to   = c("profession","years_programming","age"))#,"is_fast"))
#adjusted_score cannot be parent of anyone
blacklist_5 <- data.frame(from = c("adjusted_score"),
                          to   = node.names[-grep("adjusted_score", node.names)])

#Task Accuracy can only be measured with all tasks data. 
#Here we are dealing only with programmer demographic data.

blacklist_all <- rbind(blacklist_1,blacklist_3,blacklist_4,blacklist_5) 

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
blacklist_all <- blacklist_all[!(blacklist_all$from %in% c("is_fast") ),]
blacklist_all <- blacklist_all[!(blacklist_all$to %in% c("is_fast") ),]


#Run structure discovery for each profession
professions = c("Other", "Undergraduate_Student","Graduate_Student","Hobbyist",
                "Programmer","Professional")

#Constraint-Based Algorithm
for (i in 1:length(professions)) {
  choice = professions[i]
  df_prof <- df_selected[df_selected$profession==choice,]
  df_prof <- 
    dplyr::select(df_prof,
                  years_programming,
                  age,
                  test_duration,
                  adjusted_score
    );
  bn <-pc.stable(df_prof,blacklist = blacklist_all)
  plot(bn,main=choice)
  #graphviz.plot(bn,main=choice,shape="ellipse",layout = "circo");
}

"Analysis of results of the PC algorithm
test duration seem relevant only for professional, undergrad, grad, hobbyist
only in undegrad that test duration is affected by years_programming"

#Score-based algorithm - Hill Climbing
for (i in 1:length(professions)) {
  choice = professions[i]
  df_prof <- df_selected[df_selected$profession==choice,]
  df_prof <- 
    dplyr::select(df_prof,
                  years_programming,
                  age,
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
Test duration has years_programming as parent in Hobbyist, Undergrad, 
Programmer, and Other.
"

#-------------------------------------
#USing now the qualification_score

df_selected <-
  dplyr::select(df_E2_ground,
                years_programming,
                test_duration,
                qualification_score,
                age,
                profession
  );

node.names <- colnames(df_selected)

#years_programming is not parent of age.
blacklist_1 <- data.frame(from = c("years_programming"), 
                          to   = c("age"))
#profession has parent nodes
blacklist_2 <- data.frame(from = node.names[-grep("profession", node.names)], 
                          to   = c("profession"))
#test_duration is not parent of age, years_programming, profession
blacklist_3 <- data.frame(from = c("test_duration"),
                          to   = c("profession","years_programming","age"))
#adjusted_score cannot be parent of anyone
blacklist_4 <- data.frame(from = c("qualification_score"),
                          to   = node.names[-grep("qualification_score", node.names)])

blacklist_all <- rbind(blacklist_1,blacklist_2,blacklist_3,blacklist_4) 

#------------------------------------------
#Including Profession

bn <-tabu(df_selected,blacklist = blacklist_all)
plot(bn,main="All Professions")


#Remove profession from blacklist
blacklist_all <- blacklist_all[!(blacklist_all$from %in% c("profession") ),]
blacklist_all <- blacklist_all[!(blacklist_all$to %in% c("profession") ),]

#Constraint-Based Algorithm
for (i in 1:length(professions)) {
  choice = professions[i]
  df_prof <- df_selected[df_selected$profession==choice,]
  df_prof <- 
    dplyr::select(df_prof,
                  years_programming,
                  age,
                  test_duration,
                  qualification_score
    );
  bn <-pc.stable(df_prof,blacklist = blacklist_all)
  plot(bn,main=choice)
  #graphviz.plot(bn,main=choice,shape="ellipse",layout = "circo");
}

"Analysis of results of the PC algorithm
Test duration has not effect on adjusted_score for other and Programmer
Only in undegrad that test duration is affected by years_programming
Hence, qualification_Score and adjusted_score produced the same graphs with the PC algorithm
"

#Score-based algorithm - Hill Climbing
for (i in 1:length(professions)) {
  choice = professions[i]
  df_prof <- df_selected[df_selected$profession==choice,]
  df_prof <- 
    dplyr::select(df_prof,
                  years_programming,
                  age,
                  test_duration,
                  qualification_score
    );
  bn <-tabu(df_prof,blacklist = blacklist_all)
  plot(bn,main=choice)
  #graphviz.plot(bn,main=choice,shape="ellipse",layout = "circo");
}

"Analysis of results of the Tabu algorithm
Test duration has not effect on adjusted_score for other and Programmer
Test duration has no parents for Graduate and Professional
Only in Hobbyists that test duration is a mediator for effect on adjusted_score
Test duration has years_programming as parent in Hobbyist, Undergrad, 
Programmer, and Other.

Hence, qualification_score and adjusted_score produced the same graph with the tabu algorithm
"


#TODO
#compare how professions are distinct in terms of adjusted_score and qualification_score
#Compare the strength of graph connections using adjusted_score and qualification_score

#https://arxiv.org/pdf/0908.3817.pdf

