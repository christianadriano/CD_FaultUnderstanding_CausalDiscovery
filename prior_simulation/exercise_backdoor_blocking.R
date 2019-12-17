
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
paths(dag1,from="D",to="Y",directed = FALSE)
# $paths
# [1] "B -> A -> X -> W -> Y <- D <- C -> Z"
# [2] "B -> A -> X -> W -> Y <- Z"          
# [3] "B -> A -> X <- Z"                    
# 
# $open
# [1] FALSE FALSE FALSE

#Computing direct effect from D to Y, 
#1. remove the edge D -> Y 
#2. find the d-separation set
#see Pearl Primer book pages 84 and 85.
dagstr <-  "dag {
B -> Z
B -> A
A -> X
X -> W
W -> Y
C -> Z
C -> D
Z -> X
Z -> Y
}"
adjustmentSets(dag1,exposure = "D",outcome = "Y",effect = c("direct"))
#{ W, Z } ,{ X, Z }, { A, Z },{ B, Z },{ C }
dseparated( dagstr, "D", "Y",c("Z","W"))
# [1] TRUE


#Conditional independence assumptions
paths(dag1,from="D",to="Y",directed = FALSE)
# $paths
# [1] "D -> Y"                               "D <- C -> Z -> X -> W -> Y"          
# [3] "D <- C -> Z -> Y"                     "D <- C -> Z <- B -> A -> X -> W -> Y"       
# $open
# [1]  TRUE  TRUE  TRUE FALSE


adjustmentSets(dag1,exposure = "D",outcome = "Y",effect = c("direct"))
# { W, Z }
# { X, Z }
# { A, Z }
# { B, Z }
# { C }

adjustmentSets(dag1,exposure = c("D","W"),outcome = "Y",effect = c("direct"))
# { C, X }
# { Z }

adjustmentSets(dag1,exposure = c("W"),outcome = "Y",effect = c("direct"))
# { D, Z }
# { C, Z }
# { B, Z }
# { A, Z }
# { X }


#Create
dag2 <- dagitty( "dag {
Duration -> Score
Yoe -> Score
Age -> Score
Age -> Yoe
Yoe-> Duration
Age -> Duration
}")

#Plot
coordinates(dag2) <- list( x=c(Age=0,Yoe=2,Duration=1, Score=1) , 
                           y=c(Age=0,Yoe=0,Duration=1, Score=2) )
plot( dag2)
tidy_dagitty(dag2)
ggdag(dag2, layout = "circle")

#Conditional independence assumptions
paths(dag2,from="Duration",to="Score",directed = FALSE)
# $paths
# [1] "Duration -> Score"               "Duration <- Age -> Score"       
# [3] "Duration <- Age -> Yoe -> Score" "Duration <- Yoe -> Score"       
# [5] "Duration <- Yoe <- Age -> Score"
# 
# $open
# [1] TRUE TRUE TRUE TRUE TRUE

adjustmentSets(dag2,exposure = c("Duration"),outcome = "Score",effect = c("direct"))
#{ Age, Yoe }

adjustmentSets(dag2,exposure = c("Duration","Yoe"),outcome = "Score",effect = c("direct"))
#{ Age }

adjustmentSets(dag2,exposure = c("Yoe"),outcome = "Duration",effect = c("total"))
#{ Age }

adjustmentSets(dag2,exposure = c("Age"),outcome = "Yoe",effect = c("total"))
#{  }
