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

trial_list <-  c(5000) #500,50000) #number successful of trials for the binomial distribution
grade_range = c(3:3)
grade_priors_matrix <- matrix(nrow=1, ncol=5)

# hist_prior <- hist(rnbinom(5,mu=1,size=1),plot=FALSE)
# grade_priors_matrix <- rbind(grade_priors_matrix,hist_prior$counts)
i <- 0
#Simulate priors for Grade
for(grade_mu in grade_range){
  for(trial in trial_list){
    prior <- rnbinom(5,mu=grade_mu,size=trial)
    hist_prior <- hist(prior, plot=FALSE )
    grade_priors_matrix <- rbind(grade_priors_matrix,hist_prior$counts)
  }
}
nrows <- dim(grade_priors_matrix)[1]
grade_priors_matrix <- grade_priors_matrix[2:nrows,]
grade_priors_matrix

barplot(grade_priors_matrix,beside = TRUE,
        names.arg = c(0:5))
          
          #round(hist_prior$breaks[-length(hist_prior$breaks)]))


#h_grade_prior <- dens(grade_priors_matrix[1:10,],plot=TRUE)
#h_grade_prior <- dens(grade_priors_matrix,breaks = hist_prior$breaks,plot=TRUE)


#barplot(grade_priors_matrix,
#        beside = TRUE,
#        names.arg = round(h_grade_prior$breaks[-length(h_grade_prior$breaks)]))


#Continuous priors
grade_mu <-  rnorm(1e4,3,1)
grade_sigma <-  runif(1e4,0,1)
#grade_prior <-  dnorm(1e4,grade_mu,grade_sigma)

dens(grade_prior)
