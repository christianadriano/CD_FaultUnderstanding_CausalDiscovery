"
Studying hidden confounders while derivating NDE (Natural Direct Effect)

Section 2.1 of Pearl´s paper [1]

[1] Pearl, Judea. The do-calculus revisited. arXiv preprint arXiv:1210.4852 (2012).
url: https://arxiv.org/pdf/1210.4852
"
library( dagitty )
library(ggdag)

#Causal graph
#Create
dag1 <- dagitty( "dag {
Y [outcome];
X [exogenous];
M [endogenous];
W2 [exogenous];
W2_H [hidden_confounder];
W3 [endogenous];
W3_H [hidden_confouder];
X -> Y;
X -> M;
W3 -> Y;
W2 -> X;
M -> Y;
W2_H -> M;
W2_H -> W2;
W3_H -> W3;
W3_H -> X
}")

coordinates(dag1) <- list( x=c(Y=3,X=0,M=1,W2=0,W3=2,W2_H=0,W3_H=1) ,
                           y=c(Y=2,X=2,M=0,W2=1,W3=3,W2_H=0,W3_H=4))
plot( dag1 )
tidy_dagitty(dag1)
ggdag(dag1, layout = "circle")

condIndep <- impliedConditionalIndependencies(dag1)
condIndep #{} 

#Conditional independence assumptions
paths(dag1, from=c("X"),to="Y",directed = TRUE)
# $paths [1] "X -> M -> Y" "X -> Y"     
# $open [1] TRUE TRUE



#-------------------------------
#Intervention on Upvotes and Rank -> Effect on Replies
adjustmentSets(dag1, exposure = c("X"), outcome = "Y", 
               type = c("minimal"), effect = c("direct"))
#DIRECT: {length} =>> E[replies|do(upvotes=U,rank=K),length]
#TOTAL: No Adjustment needed =>> E[replies|do(upvotes=U,rank=K)]



#---------------------------------------------------------------

dag2 <- dagitty( "dag {
Y [outcome];
X [exogenous];
M [endogenous];
W2 [exogenous];
W2_H [hidden_confounder];
W3 [endogenous];
X -> Y;
X -> M;
W3 -> Y;
W2 -> X;
W2_H -> M;
W2_H -> W2;
M -> Y
}")

coordinates(dag2) <- list( x=c(Y=3,X=0,M=1,W2=0,W3=2, W2_H=0),#,W3_H=1) ,
                           y=c(Y=2,X=2,M=0,W2=1,W3=3,W2_H=0)) #W3_H=4))
plot( dag2 )
tidy_dagitty(dag2)

adjustmentSets(dag2, exposure = c("X"), outcome = "Y", 
               type = c("minimal"), effect = c("total"))
