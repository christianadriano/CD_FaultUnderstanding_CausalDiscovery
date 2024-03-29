"
#Item Response Theory Model for Experiment -1
## What is it? 
## Why is it important?
## What did it show?

"


library(dplyr)
library(ltm)
library(psych)
library(mirt)
library(farff)
library(ggplot2)
library(scales)

"LOAD FILES"
path <- "C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data//"
dataset_E1 <- readARFF(paste0(path,"consent_consolidated_Experiment_1.arff"))
df_consent <- data.frame(dataset_E1)
dim(df_consent) #4776   16

"Remove participants for whom we did not take the qualification test, NA's in the column" 
df <- df_consent[complete.cases(df_consent[,"qualification_score"]),] #left with 3699 rows
dim(df) #3699   21, hence there were 1077 participants (4776-3699), who did not take the qualification test

"Replace false for 0(zero) and true for 1(one)"
df$test1_ <-  ifelse(df$test1=="true",1,0)
df$test2_ <-  ifelse(df$test2=="true",1,0)
df$test3_ <-  ifelse(df$test3=="true",1,0)
df$test4_ <-  ifelse(df$test4=="true",1,0)


#df_tests <- dplyr::filter(df,!c(test1_==0 & test2_==0 & test3_==0 & test4_==0))
df_tests <- df %>% dplyr::select(test1_,test2_,test3_,test4_)


#------------------------------------------------------
"Run the 2PL model, only difficulty and discrimination"

IRT_model_2PL <- ltm(df_tests ~ z1, IRT.param=TRUE)

IRT_model_2PL
#         Dffclt    Dscrmn
# test1_ 1.8085255 1.3342712
# test2_ 0.6030424 0.3566581
# test3_ 0.9294014 7.5319611
# test4_ 2.3272393 0.2682696

"Coefficients for dffclt show the test was difficult. While test 1 was both the second
most difficult and second most dicriminative, the most difficult test (test 4) was the
lowest in dicriminative power. Instead, test 3 was the most discriminative. 

This can be visually confirmed by the inclination of the sigmoids of 
the item characteristics curves in the figure below. 
The steepest ones (test 1 and test 3) are the most discriminative)

These results show that difficulty and discrimination are always correlated metrics.
"

plot(IRT_model_2PL, type="ICC")

plot(IRT_model_2PL, type="ICC", items=c(1,3))

"Plot tells which are the in the x-axis gives me more information
in terms of discrimination power of the items (all items). 
This is important to show design the items
in a way that they focus more or less on certain parameter
configurations, which in the case of the example is 
ability. 

The plot shows the test information covers from 0 to 2 with peak on one 
standard deviation of the ability, which has narrower distribution 
compared with the distribution of test in E2. This means that the 
E2 qualification test could discriminate among a wider spectrum of programming ability
than the E1 qualification test. This was not accident, because, based on the outcomes
of E1, I planned the E2 qualification test to have 5 questions (instead of 4) to 
be more difficult.
"
plot(IRT_model_2PL, type="IIC", items=0)


#--------

factors <- factor.scores.ltm(IRT_model_2PL)
factors
# Scoring Method: Empirical Bayes
# Factor-Scores for observed response patterns:
#     test1_ test2_ test3_ test4_  Obs      Exp     z1 se.z1
# 1       0      0      0      0 1012 1106.012 -0.313 0.934
# 2       0      0      0      1  590  530.431 -0.083 0.914
# 3       0      0      1      0  124   97.857  1.087 0.294
# 4       0      0      1      1   76   74.007  1.111 0.307
# 5       0      1      0      0  878  777.594 -0.010 0.902
# 6       0      1      0      1  335  394.506  0.194 0.834
# 7       0      1      1      0  134  125.219  1.120 0.313
# 8       0      1      1      1   54   96.469  1.147 0.331
# 9       1      0      0      0  112   83.603  0.521 0.522
# 10      1      0      0      1   74   47.504  0.584 0.450
# 11      1      0      1      0   29   56.306  1.243 0.412
# 12      1      0      1      1   25   46.043  1.294 0.463
# 13      1      1      0      0   43   73.299  0.601 0.432
# 14      1      1      0      1   18   42.869  0.646 0.387
# 15      1      1      1      0   68   80.037  1.314 0.484
# 16      1      1      1      1  127   67.244  1.387 0.563

"Factor scores show that the most frequent combination (Obs) was
people who did not get any tests correct (1012), only test2 (878),
only test4 (590). These last two groups show how test 2 and test 4
are the ones with flatter slope, i.e., lower discrimination value 
than test 1 and test3. Note, however, that test4 was considered the most
difficulty, whereas test2 the easiest. This shows that both difficulty 
and discrimination are important to evaluate how the test is able to
evaluate a opulation over a spectrum of ability."

"Exp = is the expected number of people who were predicted to fall 
into that response pattern"

"z1 = is the difficulty of the response pattern taking into account that 
certain items are more difficult than others. This explains why this column 
is not perfectly ordered."

#----------------------------------------------------------------
# ALIGN AND RESCALE
# align to start in zero and rescale to fit the original qualification score

df_score.dat <- data.frame(factors$score.dat)

colnames(df_score.dat)[7] <- "irt_qualification_score"
colnames(df_score.dat)[8] <- "irt_qualification_score.standard_error"

shift <-min(df_score.dat$irt_qualification_score) 
if(shift<0){
  df_score.dat$irt_qualification_score <-df_score.dat$irt_qualification_score + abs(shift)
} else if(shift>0){
  df_score.dat$irt_qualification_score <- df_score.dat$irt_qualification_score - abs(shift)
}


#rescale to fit the original qualification score between zero and four
df_score.dat$irt_qualification_score <- scales::rescale(df_score.dat$irt_qualification_score,to=c(0,4))

#--------------------------------------
# WRITE IRT SCORE TO FILE

#LEFT JOIN to associate the new difficulty scores (irt_qualification_score) to the participants.
df_new <- left_join(df,df_score.dat,by=c("test1_"="test1_","test2_"="test2_","test3_"="test3_","test4_"="test4_"))

#Store in the original file the new difficulty scores (irt_qualification_score) of the participants
write.csv(df_new,"C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data//irt//E1_QualificationTest_IRT.csv")

#---------------------------------
# COMPARING DISTRIBUTIONS

plot(df_new$years_programming, df_new$irt_qualification_score)
plot(df_new$years_programming, df_new$qualification_score)

#There are three participants who somehow did not pass the exam score<2, 
#but managed to answer the survey, for this reason there are three datapoints with score 1 and 0.

#---------------------------------
# CORRELATION

cor.test(df_new$years_programming, df_new$irt_qualification_score, method="kendall")
#z = 5.1085, p-value = 3.247e-07, tau=0.1745489 >>>>MEDIUM CORRELATION
cor.test(df_new$years_programming, df_new$qualification_score, method="kendall")
#z = 5.1603, p-value = 2.466e-07, tau=0.1945103 >>>>MEDIUM CORRELATION

#-------------------------------------
# FURTHER IRT ANALYSES
  
  "alpha is the discrimination of item i (tells me that for every one unit increase 
in trait, there is a alpha increase in the log(odds) probability of getting the item correct. 
Alpha equal to one is equivalente to Probability equal to 73%. Alpha =2 (P=88%), Alpha=3 (95%), 
Alpha=4 (P=98%). " 

#log(odds(p)) = alpha*trait + beta
#odds(p) = p/1-p = exp(alpha)

# > exp(1) / (1+ exp(1))
# [1] 0.7310586
# > p = exp(2) / (1+ exp(2))
# > 0.8807971
# > p = exp(3) / (1+ exp(3))
# > 0.9525741
# > p = exp(4) / (1+ exp(4))
# > 0.9820138

"The larger the alpha, more influence the trait has in the probability of getting the item correct. 
Putting in another way, more the trait explains the score. However, when the alpha is small, 
then the score is more explained by the difficulty of the item (the beta), 
which is the same for all levels of trait. This is not good because this means 
that item is not able to discriminate among different trait levels, which is what we want."

person.fit(IRT_model_2PL)


#------------------------------------
"3PL model"
IRT_model_3PL <- tpm(df, type="latent.trait", IRT.param=TRUE)

IRT_model_3PL

plot(IRT_model_3PL, type="ICC")
plot(IRT_model_3PL, type="IIC", items=0)

factor.scores.tpm(IRT_model_3PL)

person.fit(IRT_model_3PL)


anova(IRT_model_2PL,IRT_model_3PL)

# Likelihood Ratio Table
#                     AIC      BIC  log.Lik    LRT df p.value
# IRT_model_2PL 15778.58 15828.30 -7881.29                  
# IRT_model_3PL 15663.92 15738.51 -7819.96 122.66  4  <0.001

"First the p.value shows that the models are distinct (p-value<0.001)
Second, the AIC tells that the 3PL model is more parcimonious, which 
means that it has lower risk of overfitting."

#--------------------------------------------------------------

"Polytomous item - Item Factor Analysis (Multidimensional Item Response Theory)"

PolyModel <- mirt(df, model = 1, itemtype = "gpcm")
# Full-information item factor analysis with 1 factor(s).
# FAILED TO CONVERGE within 1e-04 tolerance after 500 EM iterations.
# mirt version: 1.31 
# M-step optimizer: BFGS 
# EM acceleration: Ramsay 
# Number of rectangular quadrature: 61
# Latent density type: Gaussian 
# 
# Log-likelihood = -7881.807
# Estimated parameters: 8 
# AIC = 15779.61; AICc = 15779.65
# BIC = 15829.34; SABIC = 15803.92
# G2 (7) = 193.11, p = 0
# RMSEA = 0.085, CFI = NaN, TLI = NaN
