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

# standardize variables = (zero centered, standard deviation one)
df_E2$yoe <- scale(df_E2$years_programming)
df_E2$score <- scale(df_E2$qualification_score)

#Model-1 No interactions, but only positive relation
m1_NoInter_pos <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a + by*yoe ,
    a ~ dnorm( 0 , 1 ) ,
    by ~ dlnorm( 0 , 0.5 ) , #only postive relation between yoe and score
    sigma ~ dexp(1)
  ), data = df_E2
) 

m1_NoInter_all <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a + by*yoe,
    a ~ dnorm( 0 , 0.2 ) ,
    by ~ dnorm( 0 , 0.5 ) , #negative and postive relation between yoe and score
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
experience for too little gain in score.

Now looking at the prior for by (by ~ dnorm( 0 , 0.5 ). This normal distribution with 
std=0.5 implies that 95% of the datapoints are below 1 (i.e., two standard deviations).
In other words, the prior thinks taht only 5% of the plausible slopes are more 
extreme than 1.

Since we already think that the relationship looks very weak, this prior seems to agree
with that, because it will put 95% of the values of by below 1.

Next I simulate these priors to see how they look in the outcome space.

"

precis(m1_NoInter_pos)
precis(m1_NoInter_all)
m1_NoInter <- m1_NoInter_all

#extract the prior samples
set.seed(10)
priors_1 <- extract.prior( m1_NoInter )
A_seq <- seq( from=-2 , to=5 , length.out=50 )
mu <- link( m1_NoInter , post=priors_1 , data=list( yoe=A_seq ) )

plot( NULL , xlim=c(-1,5) , ylim=c(-1,5),
      xlab="Years of Experience (std)",
      ylab="Programming Score (std)",)
title("Prior Simulation All by_sig=0.5",font=1)

for ( i in 1:50 ) lines(A_seq, mu[i,] , col=col.alpha("black",0.4))

" Simulated values of sigma 0.5 and 1.0 for
beta_yoe accepting positive and negative and
beta_yoe only positive values.
This extracted prior did not show major differences
Same for the HPDI and PI margins around the posterior

In the face of that we preferred a less informative prior 
by=drom and sigma=1
----
However, we could also opt for more informative priors under the following
circumstances:

The intercept prior should be closer to zero, because we have two standardized 
variables are standardized, which implies thatif the predictor-variable is zero, 
the expected value of the outcome should also be zero. 

Concerning the beta_y (the slope), it does not make sense to expect
negative relationship between experience and score. We would also not
expect that the relationship is too strong.

"

#Should I use dlnorm or lnorm for the prior for 'by'?

#-------------------------------------------

#POSTERIOR

m1_NoInter <- m1_NoInter_pos
"Plotting the posterior with HPDI uncertainty"
#Plot only with the uncertainty around the mean Score for each YoE
mu <- link(m1_NoInter, data = data.frame(yoe=A_seq))
plot(score ~ yoe, df_E2, type="n") #n to hide the datapoints
title("Posterior All, sig_by=0.5")
for( i in 1:100){
  points(A_seq, mu[i,], pch=14,  col=col.alpha(rangi2,0.1))
}

#----
#Plot the HPDI range around the mean Score for each YoE
mu.mean = apply(mu,2,mean)
mu.HPDI = apply(mu,2,HPDI, prob=0.89)

plot(score ~ yoe, df_E2,col=col.alpha(rangi2,0.5)) #plot raw data
title("Posterior Pos, sig=o.5 with HPDI range")

#plot the Map line and interval more visible
lines(A_seq,mu.mean)

#plot the shaded region with 89% HPDI
shade(mu.HPDI,A_seq)

#----
#Plot the shade region with the variance
sim_1 <- sim(m1_NoInter, data=list(yoe=A_seq))
mu.PI = apply(sim_1,2, PI, prob=0.89) #compute the percentile intervals

plot(score ~ yoe, df_E2,col=col.alpha(rangi2,0.5)) #plot raw data
title("Posterior with Prediction Interval (All,Sig=0.5)")

#plot the Map line and interval more visible
lines(A_seq,mu.mean)

#plot the shaded region with 89% HPDI
shade(mu.HPDI,A_seq)

#plot the shaded region with 89% PI
shade(mu.PI,A_seq)


#------------------------------------------------------

#new model only with Intercept
m1_intercept <- quap(
  alist(
    score ~ dnorm( mu , sigma ) ,
    mu <- a, 
    a ~ dnorm( 0 , 0.2 ) ,
    b ~ dlnorm( 0 , 0.5 ) ,
    sigma ~ dexp(1)
  ), data = df_E2
) 

precis(m1_intercept)

#Compare with the previous model
plot(coeftab(m1_NoInter_pos,m1_NoInter_all), par=c("a","by"),
     xlab="Estimate")


