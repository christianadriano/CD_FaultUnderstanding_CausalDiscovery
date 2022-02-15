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
library(Rgraphviz)

#Folder and Script to plot the graphs
plots_folder <- "C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//causal_discovery//E2//plots//"
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//util//GenerateGraphPlot.R")

#Load only Consent data. No data from tasks, only from demographics and qualification test
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E2.R")
df_consent <- load_consent_create_indexes()
df_consent <- rename(df_consent,progr_years=years_programming)
df_consent <- rename(df_consent,test_score=adjusted_score)
df_consent <- rename(df_consent,partic_age=age)

df_selected <-
  dplyr::select(df_consent,
                progr_years,
                partic_age,
                test_duration,
                test_score #outcome variable
  );

node.names <- colnames(df_selected)
outcomeNode <- "test_score"

#progr_years is not parent of partic_age
blacklist_1 <- data.frame(from = c("progr_years"), 
                          to   = c("partic_age"))
#test_duration is not parent of partic_age, progr_years
blacklist_2 <- data.frame(from = c("test_duration"),
                          to   = c("progr_years","partic_age")) 
#test_score cannot be parent of anyone
blacklist_3 <- data.frame(from = c("test_score"),
                          to   = node.names[-grep("test_score", node.names)])

blacklist_all <- rbind(blacklist_1,blacklist_2,blacklist_3) 

#------------------------------------------
#HC
bn <- hc(df_selected,blacklist = blacklist_all)
bn_name="E2 All Test_Score (hc)";
save_bayesian_net_plot(bayesian_net=bn,
                       outcome_node=outcomeNode,
                       plot_title=bn_name,
                       file_name=bn_name,
                       folder=plots_folder)
#TABU
bn <-tabu(df_selected,blacklist = blacklist_all)
bn_name="E2 All Test_Score (tabu)";
save_bayesian_net_plot(bayesian_net=bn,
                       outcome_node=outcomeNode,
                       plot_title=bn_name,
                       file_name=bn_name,
                       folder=plots_folder)
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
                profession,
                partic_age,
                progr_years,
                test_duration,
                test_score #outcome adjusted score
  );

df_selected$profession <- as.factor(df_selected$profession)

#Run structure discovery for each profession
professions = c("Other", "Undergraduate_Student","Graduate_Student","Hobbyist",
                "Programmer","Professional")

#PC-STABLE and IAMB.FDR
for (i in 1:length(professions)) {
  choice = professions[i]
  df_prof <- df_selected[df_selected$profession==choice,]
  df_prof <- 
    dplyr::select(df_prof,
                  partic_age,
                  progr_years,
                  test_duration,
                  test_score
    );
  #HC
  bn <-hc(df_prof,blacklist = blacklist_all)
  bn_name=paste("E2",choice," Test_Score (hc)");
  save_bayesian_net_plot(bayesian_net=bn,
                         outcome_node=outcomeNode,
                         plot_title=bn_name,
                         file_name=bn_name,
                         folder=plots_folder)
  #TABU
  bn <-tabu(df_prof,blacklist = blacklist_all)
  bn_name=paste("E2",choice," Test_Score (tabu)");
  save_bayesian_net_plot(bayesian_net=bn,
                         outcome_node=outcomeNode,
                         plot_title=bn_name,
                         file_name=bn_name,
                         folder=plots_folder)
}
"Results of Hill Climbing
years_prog -> test_duration, all except Grad_student and Professionals
years_prog -> adjusted_score, all except Grad_student
test_duration -> adjusted_score, all except Other and Programmer
age -> years_prog, all
age -> duration, none
age -> adjusted_score, all except Grad_student and Other

Results of Tabu produced the exact same results as Hill Climbing
"

#-------------------------------------------------------
#-------------------------------------------------------
#Using now the qualification_score

#---------------------------------------------------------------------
#Using now the qualification_score

df_consent <- rename(df_consent,orig_score=qualification_score)

df_selected <-
  dplyr::select(df_consent,
                years_prog,
                age,
                test_duration,
                orig_score #outcome
  );


node.names <- colnames(df_selected)

#years_prog is not parent of age.
blacklist_1 <- data.frame(from = c("years_prog"), 
                          to   = c("age"))
#test_duration is not parent of age, years_prog
blacklist_2 <- data.frame(from = c("test_duration"),
                          to   = c("years_prog","age"))
#orig_score cannot be parent of anyone
blacklist_3 <- data.frame(from = c("orig_score"),
                          to   = node.names[-grep("orig_score", node.names)])

blacklist_all <- rbind(blacklist_1,blacklist_2,blacklist_3) 


#------------------------------------------
#Including Profession as Node

bn <- pc.stable(df_selected,blacklist = blacklist_all)
plot(bn,main="All Professions, pc.stable algorithm")

bn <-iamb(df_selected,blacklist = blacklist_all)
plot(bn,main="All Professions, iamb algorithm")

bn <-iamb.fdr(df_selected,blacklist = blacklist_all)
plot(bn,main="All Professions, iamb.fdr algorithm")

#-----------------------------------------
#Remove Profession as Node

df_selected <-
  dplyr::select(df_consent,
                profession,
                years_prog,
                age,
                test_duration,
                orig_score #outcome
  );

for (i in 1:length(professions)) {
  choice = professions[i]
  df_prof <- df_selected[df_selected$profession==choice,]
  df_prof <- 
    dplyr::select(df_prof,
                  years_prog,
                  age,
                  test_duration,
                  orig_score
    );
  
  #HC
  bn <-pc.stable(df_prof,blacklist = blacklist_all)
  bn_name=paste("E2",choice," Original_Test_Score (hc)");
  save_bayesian_net_plot(bayesian_net=bn,
                         outcome_node=outcomeNode,
                         plot_title=bn_name,
                         file_name=bn_name,
                         folder=plots_folder)
  #TABU
  bn <-iamb.fdr(df_prof,blacklist = blacklist_all)
  bn_name=paste("E2",choice," Original_Test_Score (tabu)");
  save_bayesian_net_plot(bayesian_net=bn,
                         outcome_node=outcomeNode,
                         plot_title=bn_name,
                         file_name=bn_name,
                         folder=plots_folder)
}
"Qualification score results were identical to Adjusted score"

#------------------------------------------------------
#SPEED CLUSTERS
#------------------------------------------------------
#Will only used TABU

df_consent <- load_consent_create_indexes()
df_consent <- rename(df_consent,progr_years=years_programming)

df_selected <-
  dplyr::select(df_consent,
                profession,
                progr_years,
                age,
                test_duration,
                orig_score #outcome
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
bn <- h2pc(df_consent_fast,blacklist = blacklist_all)
plot(bn,main="FAST Answers (Tabu algorithm)")
"FAST answers graph is identical to no clustering by answer speed.
Other: yoe -> score, age -> yoe
Undergrad: yoe -> score, age -> yoe, age->score, yoe->duration, duration->score
Grad: yoe -> score, age -> yoe, age->score
Hobbyist: yoe -> score, age -> yoe, age->score, yoe->duration, duration->score
Programmer: yoe -> score, age -> yoe,
Professional: yoe -> score, age -> yoe, age->score, duration->score
"

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

