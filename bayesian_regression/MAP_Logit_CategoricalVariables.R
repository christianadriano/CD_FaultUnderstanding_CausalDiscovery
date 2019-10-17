"
Interaction model for categorical predictors and outcome
- Categorical input: Profession, Grade
- Continous input: code complexity
- Categorical output: difficulty
"

library(stringr)
library(rethinking)

path <- "C://Users//Christian//Documents//GitHub//Complexity_Metrics//output//"
dataset_E2 <- read.csv(str_c(path, "merged_tasks_complexity_E2.csv"))
df_E2 <- data.frame(dataset_E2)
summary(df_E2)

size_list <-  c(1,10,100,1000) #number successful of trials for the binomial distribution
i <- 0
grade_priors_matrix <- matrix(nrow=6,ncol=4)
#Simulate priors for Grade
for(mu in 0:5){
  for(size in size_list){
    prior <- rnbinom(1500,mu=mu,size=1000)
    
    grade_priors_matrix[i,j] <- prior$count    
    grade_priors_counts[i] <- prior$counts
    
    i <- i+1
    rbind()
  }
}

h_grade_prior <- dens(grade_prior,breaks = h1$breaks,plot=TRUE)
barplot(h_grade_prior$counts,
        beside = TRUE,
        names.arg = round(h_grade_prior$breaks[-length(h_grade_prior$breaks)]))


#Continuous priors
grade_mu <-  rnorm(1e4,3,1)
grade_sigma <-  runif(1e4,0,1)
#grade_prior <-  dnorm(1e4,grade_mu,grade_sigma)

dens(grade_prior)
