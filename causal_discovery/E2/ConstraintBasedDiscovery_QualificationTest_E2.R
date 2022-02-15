
"
Constraint-Based discovery of Qualification Test causal graph

profession [exogenous];
years_programming [exogenous];
age [exogenous]
test_duration [endogenous];
adjusted_score [outcome];

Constraint-based algorithms execute a conditional independence tests
induced by each hypothetical graph. Because these tests are executed
sequentially (i.e., multiple times), there is a risk of false positives (i.e., false discovery)
by compounding errors. Therefore, algorithms try to minimize this risk
in various ways. To test the sensitivity of our data to this risk
we executed methods with increasingly power to mitigate false positives.

\begin{list}
\item PC (pc.stable), seminal constraint-based structure learning algorithm \cite{colombo2014order}
#Colombo D, Maathuis MH (2014). Order-Independent Constraint-Based Causal Structure Learning. Journal of Machine Learning Research, 15:3921–3962.

\item Incremental Association (iamb) is based on the Markov blanket detection algorithm \cite{tsamardinos2003algorithms} that executes a 
two-phase selection, more specifically a forward selection followed by a iteration to remove false positives.

Tsamardinos I, Aliferis CF, Statnikov A (2003). Algorithms for Large Scale Markov Blanket Discovery. Proceedings of the Sixteenth International Florida Artificial Intelligence Research Society Conference, 376–381.

\item Incremental Association with FDR (iamb.fdr) is an improvement on the IAMB. It adjusts the tests 
significance threshold with false discovery rate heuristics \cite{pena2008learning,gasse2014hybrid}

Pena JM (2008). Learning Gaussian Graphical Models of Gene Networks with False Discovery Rate Control. Proceedings of the Sixth European Conference on Evolutionary Computation, Machine Learning and Data Mining in Bioinformatics, 165–176.

Gasse M, Aussem A, Elghazel H (2014). A Hybrid Algorithm for Bayesian Network Structure Learning with Application to Multi-Label Learning. Expert Systems with Applications, 41(15):6755–6772.

\end{list}

The library used was bnlearn \cite{scutari2010learning}
https://www.bnlearn.com/documentation/man/structure.learning.html
Learning Bayesian Networks with the bnlearn R - https://arxiv.org/pdf/0908.3817.pdf


##Summary of results

Draw hypothetical graph (prior)
Specify the query on duration and YoE (the reflect on causal discovery)

The hypothetical graph assumed that both YoE and Duration are causes of the test score.
Age is a cause of YoE. We also believed that YoE should be a cause of Duration,
as more experienced people would take less time to comprehend the program and complete the test questions.
However, we found surprising results:

1- Duration is detected as a cause of Score for all professions, except Other and Graduate Students. 

2- YoE was detected as a cause of Duration only for undergraduate students. 

3- YoE was detected as a cause of Score, except for Graduate students.

Ultimately, as expected, Age was detected as a causal effect of YoE. Although Age, as well as Gender and
Country of origin are not features that we selected, the association between Age and YoE contributes to
increase confidence on the trustworthiness of the self-reported demographics data.


For each of these findings, we investigate if they generalize across YoE strata and 
compute the effect size of the causal associations that were not ruled out.

#Categorical variables are not supported, so need to run the analysis by profession and speed_cluster

#Discussion of results
#compare graphs produced by each of the methods. Check how sensitive they are to false discovery rate.
#compare how professions are distinct in terms of adjusted_score and qualification_score
Results from adjusted_score and qualification score are the same across professions.

#compare between speed_clusters

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
#PC.STABLE
bn <- pc.stable(df_selected,blacklist = blacklist_all)
bn_name="E2 All Test_Score (pc.stable)";
save_bayesian_net_plot(bayesian_net=bn,
                       outcome_node=outcomeNode,
                       plot_title=bn_name,
                       file_name=bn_name,
                       folder=plots_folder)
#IAMB.FDR
bn <-iamb.fdr(df_selected,blacklist = blacklist_all)
bn_name="E2 All Test_Score (iamb.fdr)";
save_bayesian_net_plot(bayesian_net=bn,
                       outcome_node=outcomeNode,
                       plot_title=bn_name,
                       file_name=bn_name,
                       folder=plots_folder)

"
All algorithms produced the same graph.
prog_years nodes is only connected to test_score by an undirected edge
age -> test_duration
age -> prog_years
test_duration -> prog_years
no edge between prog_years and test_duration
"

#-----------------------------------------
#-----------------------------------------
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
  #PC.STABLE
  bn <-pc.stable(df_prof,blacklist = blacklist_all)
  bn_name=paste("E2",choice," Test_Score (pc.stable)");
  save_bayesian_net_plot(bayesian_net=bn,
                         outcome_node=outcomeNode,
                         plot_title=bn_name,
                         file_name=bn_name,
                         folder=plots_folder)
  #IAMB.FDR
  bn <-iamb.fdr(df_prof,blacklist = blacklist_all)
  bn_name=paste("E2",choice," Test_Score (iamb.fdr)");
  save_bayesian_net_plot(bayesian_net=bn,
                         outcome_node=outcomeNode,
                         plot_title=bn_name,
                         file_name=bn_name,
                         folder=plots_folder)
}


"All two algorithms produced the same results with one exception.
For Hobbyist, the IAMB.FDR determined that Age->Duration
For all professions Age -> YoE
Only for undergrads YoE -> Test_Duration
All (except Programmer and Other) Test_Duration -> Adjusted_Score
All (except Graduate_Student) YoE -> Adjusted_Score
Only for undergraduate, Age -> Adjusted_Score. 
"

#---------------------------------------------------------------------
#---------------------------------------------------------------------
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

#Remove profession from blacklist
blacklist_all <- blacklist_all[!(blacklist_all$from %in% c("profession") ),]
blacklist_all <- blacklist_all[!(blacklist_all$to %in% c("profession") ),]

df_selected <-
  dplyr::select(df_consent,
                profession,
                years_prog,
                age,
                test_duration,
                orig_score #outcome
  );

#PC-STABLE and IAMB.FDR
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

  #PC.STABLE
  bn <-pc.stable(df_prof,blacklist = blacklist_all)
  bn_name=paste("E2",choice," Original_Test_Score (pc.stable)");
  save_bayesian_net_plot(bayesian_net=bn,
                         outcome_node=outcomeNode,
                         plot_title=bn_name,
                         file_name=bn_name,
                         folder=plots_folder)
  #IAMB.FDR
  bn <-iamb.fdr(df_prof,blacklist = blacklist_all)
  bn_name=paste("E2",choice," Original_Test_Score (iamb.fdr)");
  save_bayesian_net_plot(bayesian_net=bn,
                         outcome_node=outcomeNode,
                         plot_title=bn_name,
                         file_name=bn_name,
                         folder=plots_folder)
}

"Analysis of results of the PC algorithm
Test duration has not effect on adjusted_score for other and Programmer
Only in undegrad that test duration is affected by years_prog
Hence, qualification_Score and adjusted_score produced the same graphs with the PC algorithm
"

#IAMB
for (i in 1:length(professions)) {
  choice = professions[i]
  df_prof <- df_selected[df_selected$profession==choice,]
  df_prof <- 
    dplyr::select(df_prof,
                  years_prog,
                  partic_age,
                  test_duration,
                  orig_score
    );
  bn <-iamb(df_prof,blacklist = blacklist_all)
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

#IAMB-FDR
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
  bn <-iamb.fdr(df_prof,blacklist = blacklist_all)
  plot(bn,main=choice)
}

#------------------------------------------------------
#SPEED CLUSTERS
#------------------------------------------------------
#Will only used the IAM.FDR

df_consent <- load_consent_create_indexes()

df_consent <- rename(df_consent,years_prog=years_programming)

#Evaluate how fast and slow can explain adjusted_score score
df_consent_fast <- df_consent[df_consent$is_fast,]
df_consent_slow <- df_consent[!df_consent$is_fast,]

df_selected <-
  dplyr::select(df_consent_fast,
                profession,
                years_prog,
                age,
                test_duration,
                adjusted_score #outcome
  );

node.names <- colnames(df_selected)

#years_prog is not parent of age.
blacklist_1 <- data.frame(from = c("years_prog"), 
                          to   = c("age"))
#test_duration is not parent of age, years_prog, profession
blacklist_2 <- data.frame(from = c("test_duration"),
                          to   = c("years_prog","age")) #"profession",
#adjusted_score cannot be parent of anyone
blacklist_3 <- data.frame(from = c("adjusted_score"),
                          to   = node.names[-grep("adjusted_score", node.names)])

blacklist_all <- rbind(blacklist_1,blacklist_2,blacklist_3) 

#Remove profession from blacklist
blacklist_all <- blacklist_all[!(blacklist_all$from %in% c("profession") ),]
blacklist_all <- blacklist_all[!(blacklist_all$to %in% c("profession") ),]

#IAMB-FDR FAST
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
  bn <-iamb.fdr(df_prof,blacklist = blacklist_all)
  plot(bn,main=paste(choice,"(fast answers)"))
}

"Fast answer cluster has same results as no clustering"

df_selected <-
  dplyr::select(df_consent_slow,
                profession,
                years_prog,
                age,
                test_duration,
                adjusted_score #outcome
  );

#IAMB-FDR SLOW
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
  bn <-iamb.fdr(df_prof,blacklist = blacklist_all)
  plot(bn,main=paste(choice,"(slow answers)"))
}

"Slow clustering showed only one edge Age->YoE for Hobbyists"