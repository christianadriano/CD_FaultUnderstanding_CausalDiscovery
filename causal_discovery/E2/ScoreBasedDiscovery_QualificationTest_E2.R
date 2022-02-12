"
Score-Based Causal discovery for the programming skill and test Duration related factors

profession [exogenous];
years_programming [exogenous];
age [exogenous]
test_duration [endogenous];
qualification_score [outcome];
adjusted_score [outcome];


\begin{list}

\item Hill-Climbing (hc): a hill climbing greedy search that explores the space of the directed acyclic graphs by single-arc addition, removal and reversals; with random restarts to avoid local optima. The optimized implementation uses score caching, score decomposability and score equivalence to reduce the number of duplicated tests.

\item Tabu Search (tabu): a modified hill-climbing able to escape local optima by selecting a network that minimally decreases the score function.

\end{list}

Pena JM (2008). Learning Gaussian Graphical Models of Gene Networks with False Discovery Rate Control. Proceedings of the Sixth European Conference on Evolutionary Computation, Machine Learning and Data Mining in Bioinformatics, 165–176.

Gasse M, Aussem A, Elghazel H (2014). A Hybrid Algorithm for Bayesian Network Structure Learning with Application to Multi-Label Learning. Expert Systems with Applications, 41(15):6755–6772.


The library used was bnlearn \cite{scutari2010learning}
https://www.bnlearn.com/documentation/man/structure.learning.html
Learning Bayesian Networks with the bnlearn R - https://arxiv.org/pdf/0908.3817.pdf

#TODO

#Draw hypothetical graph (prior)

#Profession not supported, so need to run the analysis by profession
#Include speed_fast/slow
#Specify the query on duration and YoE (the reflect on causal discovery)

#compare graphs produced by each of the methods. Check how sensitive they are to false discovery rate.
#compare how professions are distinct in terms of adjusted_score and qualification_score

" 

library(bnlearn)
library(dplyr)

#Load only Consent data. No data from tasks, only from demographics and qualification test
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E2.R")
df_consent <- load_consent_create_indexes()
df_consent <- rename(df_consent,years_prog=years_programming)

df_selected <-
  dplyr::select(df_consent,
                years_prog,
                age,
                test_duration,
                adjusted_score #outcome
                );

node.names <- colnames(df_selected)

#years_prog is not parent of age.
blacklist_1 <- data.frame(from = c("years_prog"), 
                          to   = c("age"))

#test_duration is not parent of age, years_prog
blacklist_2 <- data.frame(from = c("test_duration"),
                          to   = c("years_prog","age"))
#adjusted_score cannot be parent of anyone
blacklist_3 <- data.frame(from = c("adjusted_score"),
                          to   = node.names[-grep("adjusted_score", node.names)])

blacklist_all <- rbind(blacklist_1,blacklist_2,blacklist_3) 

#----------------------------------------------
bn_hc <- hc(df_selected,blacklist = blacklist_all)
plot(bn_hc,main="All Professions, hill climbing algorithm")

bn_tabu <-tabu(df_selected,blacklist = blacklist_all)
plot(bn_tabu,main="All Professions, tabu algorithm")

"
Both algorithms produced the same graph. It has two additional 
edges than the graph discovered by the CB algorithms. 
The two additional edges are years_prog->test_duration, age->adjusted_score
"

#----------------------------------------------
#----------------------------------------------
#BY PROFESSION

df_selected <-
  dplyr::select(df_consent,
                profession, #not using because it is categorical and the current methods do not support it
                years_prog,
                age,
                test_duration,
                adjusted_score #outcome
  );


#Run structure discovery for each profession
professions = c("Other", "Undergraduate_Student","Graduate_Student","Hobbyist",
                "Programmer","Professional")

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
                  test_duration,
                  adjusted_score
    );
  bn <-hc(df_prof,blacklist = blacklist_all)
  plot(bn,main=choice)
}

"Results of Hill Climbing
years_prog -> test_duration, all except Grad_student and Professionals
years_prog -> adjusted_score, all except Grad_student
test_duration -> adjusted_score, all except Other and Programmer
age -> years_prog, all
age -> duration, none
age -> adjusted_score, all except Grad_student and Other
"

#Score-based algorithm - Tabu
for (i in 1:length(professions)) {
  choice = professions[i]
  df_prof <- df_selected[df_selected$profession==choice,]
  df_prof <- 
    dplyr::select(df_prof,
                  years_prog,
                  age,
                  test_duration,
                  adjusted_score
    );
  bn <-tabu(df_prof,blacklist = blacklist_all)
  plot(bn,main=choice)
}

"Results of Tabu produced the exact same results as Hill Climbing"

#-------------------------------------------------------
#-------------------------------------------------------
#Using now the qualification_score

df_selected <-
  dplyr::select(df_consent,
                age,
                years_prog,
                test_duration,
                qualification_score #outcome
  );


node.names <- colnames(df_selected)

#years_prog is not parent of age.
blacklist_1 <- data.frame(from = c("years_prog"), 
                          to   = c("age"))

#test_duration is not parent of age, years_prog
blacklist_2 <- data.frame(from = c("test_duration"),
                          to   = c("years_prog","age"))
#qualification_score cannot be parent of anyone
blacklist_3 <- data.frame(from = c("qualification_score"),
                          to   = node.names[-grep("qualification_score", node.names)])

blacklist_all <- rbind(blacklist_1,blacklist_2,blacklist_3) 

#------------------------------------------

bn <- hc(df_selected,blacklist = blacklist_all)
plot(bn,main="All Professions, Hill-Climbing")

bn <-tabu(df_selected,blacklist = blacklist_all)
plot(bn,main="All Professions, Tabu")

"Identical results"

#--------------------------
#Including Profession

df_selected <-
  dplyr::select(df_consent,
                profession, #included only for building models for each profession
                age,
                years_prog,
                test_duration,
                qualification_score #outcome
  );

#Score-based algorithm - Hill Climbing
for (i in 1:length(professions)) {
  choice = professions[i]
  df_prof <- df_selected[df_selected$profession==choice,]
  df_prof <- 
    dplyr::select(df_prof,
                  years_prog,
                  age,
                  test_duration,
                  qualification_score
    );
  bn <-hc(df_prof,blacklist = blacklist_all)
  plot(bn,main=choice)
}

#Score-based algorithm - Tabu algorithm
for (i in 1:length(professions)) {
  choice = professions[i]
  df_prof <- df_selected[df_selected$profession==choice,]
  df_prof <- 
    dplyr::select(df_prof,
                  years_prog,
                  age,
                  test_duration,
                  qualification_score
    );
  bn <-tabu(df_prof,blacklist = blacklist_all)
  plot(bn,main=choice)
}

"Qualification score results were identical to Adjusted score"

#------------------------------------------------------
#SPEED CLUSTERS
#------------------------------------------------------
#Will only used TABU

df_consent <- load_consent_create_indexes()
df_consent <- rename(df_consent,years_prog=years_programming)

df_selected <-
  dplyr::select(df_consent,
                years_prog,
                age,
                test_duration,
                adjusted_score #outcome
  );


node.names <- colnames(df_selected)

#years_prog is not parent of age.
blacklist_1 <- data.frame(from = c("years_prog"), 
                          to   = c("age"))
#test_duration is not parent of age, years_prog,
blacklist_2 <- data.frame(from = c("test_duration"),
                          to   = c("years_prog","age")) 
#adjusted_score cannot be parent of anyone
blacklist_3 <- data.frame(from = c("adjusted_score"),
                          to   = node.names[-grep("adjusted_score", node.names)])

blacklist_all <- rbind(blacklist_1,blacklist_2,blacklist_3) 

#------------------------------------------

#Evaluate how fast and slow can explain adjusted_score score
df_consent_fast <- df_consent[df_consent$is_fast,]
df_consent_fast <-
  dplyr::select(df_consent_fast,
                years_prog,
                age,
                test_duration,
                adjusted_score #outcome
  );
bn <- tabu(df_consent_fast,blacklist = blacklist_all)
plot(bn,main="FAST Answers (Tabu algorithm)")
"FAST answers graph is identical to no clustering by answer speed. "

df_consent_slow <- df_consent[!df_consent$is_fast,]
df_consent_slow <-
  dplyr::select(df_consent_slow,
                years_prog,
                age,
                test_duration,
                adjusted_score #outcome
  );
bn <-tabu(df_consent_slow,blacklist = blacklist_all)
plot(bn,main="SLOW Answers (Tabu algorithm)")

"
SLOW answer present only the folling two edges
year_prog->adjusted_score, years_prog -> test_duration
"

#---------------------------------
#Analysis of SPEED cluster by Profession

#TABU FAST
df_consent_fast <- df_consent[df_consent$is_fast,]
for (i in 1:length(professions)) {
  choice = professions[i]
  df_prof <- df_selected[df_consent_fast$profession==choice,]
  df_prof <- 
    dplyr::select(df_prof,
                  years_prog,
                  age,
                  test_duration,
                  adjusted_score
    );
  bn <-tabu(df_prof,blacklist = blacklist_all)
  plot(bn,main=paste(choice,"(fast answers)"))
}

"Fast answer cluster has same results as no clustering"


#TABU SLOW
df_consent_fast <- df_consent[!df_consent$is_fast,]
for (i in 1:length(professions)) {
  choice = professions[i]
  df_prof <- df_selected[df_consent_slow$profession==choice,]
  df_prof <- 
    dplyr::select(df_prof,
                  years_prog,
                  age,
                  test_duration,
                  adjusted_score
    );
  bn <-tabu(df_prof,blacklist = blacklist_all)
  plot(bn,main=paste(choice,"(slow answers)"))
}

"Slow answer clustering graphs showed no edges"

#-------------------------------------------------------

#Remove profession from blacklist
#blacklist_all <- blacklist_all[!(blacklist_all$from %in% c("profession") ),]
#blacklist_all <- blacklist_all[!(blacklist_all$to %in% c("profession") ),]

