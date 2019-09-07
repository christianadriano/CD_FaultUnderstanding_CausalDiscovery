" 
GRID method for finding the posterior distribution.
Code inspired on McElreath book Statitsical Rethinking

https://github.com/rmcelreath/rethinking

"

library(rethinking)

#define the grid
p_grid <-  seq(from=0, to=1,length.out=20) 

#flat prior
prior <- rep(1,20)

#compute likelihood at value in the grid
likelihood <- dbinom(6, size=9, prob=p_grid)

unstandardized_posterior <- likelihood * prior

standardized_posterior <- unstandardized_posterior / sum(unstandardized_posterior)


plot(p_grid, posterior, type="b",
     xlab="probability of water", 
     ylab="posterior probability")
mtext("20 points")
