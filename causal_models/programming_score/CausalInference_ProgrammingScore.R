"
Causal inference models of the programming test score.
"

library(rethinking)
library(stringr)

install.packages("ggm")
install.packages("ggdag")
library(ggdag)
library( dagitty )
library (ggm)


#Load data
path <- "C://Users//Christian//Documents//GitHub//Complexity_Metrics//output//"
dataset_E2 <- read.csv(str_c(path, "merged_tasks_complexity_E2.csv"))
df_E2 <- data.frame(dataset_E2)

#Causal graph
#Create
dag1.1 <- dagitty( "dag {
Prof -> Score
YoE -> Score
YoE -> Prof
}")

#Plot
coordinates(dag1.1) <- list( x=c(YoE=0,Score=1,Prof=2) , y=c(YoE=0,Score=1,Prof=0) )
plot( dag1.1 )
tidy_dagitty(dag1.1)
ggdag(dag1.1, layout = "circle")
 
#Conditional independence assumptions
paths(dag1.1,c("YoE"),"Score",directed = TRUE)
# $paths [1] "YoE -> Prof -> Score" "YoE -> Score"        
# $open [1] TRUE TRUE

adjustmentSets(dag1.1,exposure = "Prof",outcome = "Score",effect = c("direct"))
#{ YoE } because YoE is a confounder.

"The effect of YoE on Score is influenced by the Profession, i.e., YoE in different
professions have different effects on Score. In this sense, Profession adjusts the
effect of YoE on Score. The adjustment can be via mediation and interaction (a.k.a. moderation). 

In the graph model, we can only show mediation. To investigate interaction, we need 
to build models with interaction. 

The opposite can also be true, i.e., the effect of profession on score is influenced
by the YoE, which is also a confounder. This means that to intervene in the profession to 
evaluate Score, I would need to control for YoE. 

What is interesting to know is how much of the effect of YoE on Score is:
1- additive (direct effect)
2- additive mediated (indirect effect)
3- via interacting with profession

Note the assymetry, in a sense that a person cannot easily change Profession, but can increase YoE.

The relation between YoE and Profession is also not given. We need to confirm it.
I cannot condition on the Score, because it is a collider, which as such, it opens
the path between YoE and Prof.

"

#Simulate priors

#Build causal models

# standardize variables = (zero centered, standard deviation one)
df_E2$yoe <- scale(df_E2$years_programming)
df_E2$score <- scale(df_E2$qualification_score)



#Model-1 No interactions
m1_NoInter <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a + by*yoe ,
    a ~ dnorm( 0 , 0.2 ) ,
    b ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp(1)
  ), data = df_E2
) 

#Let's reflect a bit about this regression formula
# score = a + by*yoe

"if by=1, this means that one standard deviation increase in YoE is
associated with likewise one standard deviation in score. 

To know whether or not this is strong relationship, 
I need to know how big a standard deviation of qualification score is"

sd(df_E2$years_programming)
#8.297915 years
sd(df_E2$qualification_score)
#0.8198906

"This translates do 8.2979 years of experience are necessary to increase
the score in a bit less than one (0.8198). This looks a bit too much
experience for too little gain in score..."

m1_intercept <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a, 
    a ~ dnorm( 0 , 0.2 ) ,
    b ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp(1)
  ), data = df_E2
) 

precis(m1_NoInter)
precis(m1_intercept)
plot(coeftab(m1_NoInter,m1_intercept), par=c("a","by"),
     xlab="Estimate")

#Simulate Priors
set.seed(10)
prior1 <- extract.prior( m1_NoInter )
A_seq <- seq( from=-2 , to=5 , length.out=50 )
mu <- link( m1_NoInter , post=prior1 , data=list( yoe=A_seq ) )

plot( NULL , xlim=c(-1,5) , ylim=c(-1,5),
      xlab="Years of Experience (std)",
      ylab="Programming Score (std)",)
title("Prior Simulation")

for ( i in 1:50 ) lines(A_seq, mu[i,] , col=col.alpha("black",0.4))

#Model-2 Interaction between profession and yoe

#Model-3 Interaction between profession and yoe and gender

#Model-4 Add Duration
