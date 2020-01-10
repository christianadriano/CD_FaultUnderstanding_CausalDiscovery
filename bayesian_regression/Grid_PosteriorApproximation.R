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


plot(p_grid, standardized_posterior, type="b",
     xlab="probability of water", 
     ylab="posterior probability")
mtext("20 points")


# We can answer questions like (from McElreath):
#    How much posterior probability is below some parameter value?
#    How much posterior probability is between two parameter values?
#    Which parameter value marks the lower 10% of the posterior probability?
#    Which range of parameter values contains 99% of the posterior probability?
#    Which parameter value has the highest posterior probability?
  
#For that we only need to sum over the posterior.
sum( standardized_posterior[ p_grid < 0.5 ] )
#[1] 0.1707239
#i.e., 17% of the data is below 50% probabiltiy of water

#Confidence Interval, which in Bayesian is called Credible Interval

#It can be calculated for different ranges of boundaries, for instance:
#the 50% central (between 25% and 75%). For that use the PI function.

#It can also be calculated based on the mass, for instance:
#the bottom 80% of densitiy mass or the top 80%, which will give 
#correspoding intervals for p_grid. For that use the quantile function.

#HPDI - Highest Probability Density Interval
#HPDI is the narrowest interval containing the desired probability mass

#For a discussion see page 57 of McElreath 2019 Rethinking book.

#These intervals (PI and HPDI) tend to provide similar results.
#Remember that the Bayesian estimate is not the intervals but the whole distribution.

#All fine, however..

#1- Grid approximation is not used because summing over the posterior
#is not feasible for more complex situations, i.e., multiple variables.
#
#2- Grid approximation makes less assumptions than other methods, but it does not scale well.
# As McElreath wrote in his book Statistical Rethinking (chapter 2, page 42):  
#   ...the number of unique values to consider in the grid grows exponentially with the number of parameters in the
# model increases. For a single parameter model, the problem to compute a grid of 100 or 1000 values is 
# respectively 100 and 1000 values. However, for two parameters approximated by 100 values each, that's
# already 100^2 = 10000 values to compute. For 10 parameters, the grid becomes many billions
# of values.
