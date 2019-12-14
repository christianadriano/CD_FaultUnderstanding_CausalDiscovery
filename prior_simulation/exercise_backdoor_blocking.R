
"
Blooking Back-door paths - Pearl Primer exercises
"

#Study question 3.3.1.

library(ggdag)
library( dagitty )
library (ggm)

#Causal graph
#Create
dag1 <- dagitty( "dag {
B -> Z
B -> A
A -> X
X -> W
W -> Y
C -> Z
C -> D
D -> Y
Z -> X
Z -> Y

}")

#Plot
coordinates(dag1) <- list( x=c(A=0,B=0,C=2, D=2, X=0, Y=2, Z=1, W=1) , 
                           y=c(A=1,B=0,C=0, D=1, X=2, Y=2, Z=1, W=2) )
plot( dag1)
tidy_dagitty(dag1.1)
ggdag(dag1.1, layout = "circle")

#Conditional independence assumptions
paths(dag1.1,c("YoE"),"Score",directed = TRUE)
# $paths [1] "YoE -> Prof -> Score" "YoE -> Score"        
# $open [1] TRUE TRUE

adjustmentSets(dag1.1,exposure = "Prof",outcome = "Score",effect = c("direct"))
#{ YoE } because YoE is a confounder.
