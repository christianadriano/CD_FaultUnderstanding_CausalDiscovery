"
Item response model of the programming test qualification E2
Goals of this script:
- Generates an adjusted score based on an item reponse model (@item_response_theory)
- Rescale the adjusted score to match the scale range of the original qualification score

TODO
- Fix merging bug
- Convert this file to rmd so I easily publish the charts
- Add small explanation in the beginning of the file 
- Add citations
- Revise writing

"
library(dplyr)
library(ltm)
library(psych)
library(mirt)
library(ggplot2)
library(farff)
library(scales)

#-------------------------------------
"LOAD CONSENT DATA EXPERIMENT-2"

path <- "C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//"
dataset_E2 <- readARFF(paste0(path,"//data//","consent_consolidated_Experiment_2.arff"))
df_consent <- data.frame(dataset_E2)

"Remove participants who did not take the qualification test" 
df_consent <- df_consent[complete.cases(df_consent[,"qualification_score"]),]
#Original size: 3658   21
#New size: 1788 21
#dim(df_consent) 

#NOT Necessary anymore, I already converted this data from true/false to 1/0's
#"Replace false for 0(zero) and true for one(1)"
# df_E2$test1_ <-  ifelse(df_E2$test1=="true",1,0)

df_tests <- df_consent %>% dplyr::select(test1,test2,test3,test4,test5)

#------------------------------------------
#BUILD THE IRT MODEL

IRT_model <- ltm(df_tests ~ z1, IRT.param=TRUE)

IRT_model
# Coefficients:
#         Dffclt  Dscrmn
# test1   0.018   3.230
# test2   0.022   1.598
# test3   0.233   2.386
# test4  -0.162   0.809
# test5   0.417   1.602

"Coefficients for dffclt show the test was not so difficult. 

Regarding discrimation, except for question 4, all other question 
were very discriminating.
"

plot(IRT_model, type="ICC")

"The steep inclination of the sigmoid-like curves show that, except for test-4, 
all other tests have a good discriminative power."

plot(IRT_model, type="ICC", items=c(1,2,4))

#--------
#NOT REVELANT INFORMATION??
"Plot the information, which tells me which area in the
x-axis gives me more information in terms of discrimination 
power of the items (all items). This is important to show design the items
in a way that they focus more or less on certain parameter
configurations, which in the case of the example is 
ability. 
"
#plot(IRT_model, type="IIC", items=0)
"The plot shows the test information covers from -4 to +4 with peak at zero."
#---------

factors <- factor.scores.ltm(IRT_model)
factors
# Scoring Method: Empirical Bayes
# Factor-Scores for observed response patterns:
#     test1 test2 test3 test4 test5 Obs   Exp     z1 se.z1
# 1      0     0     0     0     0 271 320.220 -0.966 0.642
# 2      0     0     0     0     1  56  41.950 -0.475 0.481
# 3      0     0     0     1     0 159 164.245 -0.683 0.546
# 4      0     0     0     1     1  34  32.191 -0.305 0.438
# 5      0     0     1     0     0  80  31.855 -0.310 0.440
# 6      0     0     1     0     1  12  11.481 -0.033 0.400
# 7      0     0     1     1     0  19  28.424 -0.164 0.413
# 8      0     0     1     1     1   4  13.259  0.095 0.397
# 9      0     1     0     0     0  91  79.162 -0.476 0.481
# 10     0     1     0     0     1  21  21.535 -0.163 0.413
# 11     0     1     0     1     0  90  60.698 -0.306 0.439
# 12     0     1     0     1     1  51  22.033 -0.030 0.400
# 13     0     1     1     0     0  12  21.621 -0.034 0.400
# 14     0     1     1     0     1   3  12.806  0.220 0.402
# 15     0     1     1     1     0   7  24.954  0.094 0.397
# 16     0     1     1     1     1   9  18.947  0.355 0.417
# 17     1     0     0     0     0  64  40.740 -0.158 0.413
# 18     1     0     0     0     1  17  19.213  0.100 0.397
# 19     1     0     0     1     0  69  41.880 -0.025 0.400
# 20     1     0     0     1     1  19  25.218  0.229 0.403
# 21     1     0     1     0     0  31  24.556  0.225 0.403
# 22     1     0     1     0     1   6  24.412  0.507 0.443
# 23     1     0     1     1     0  31  36.512  0.361 0.418
# 24     1     0     1     1     1  37  48.471  0.679 0.481
# 25     1     1     0     0     0  24  36.160  0.100 0.397
# 26     1     1     0     0     1  15  27.767  0.362 0.418
# 27     1     1     0     1     0  22  47.432  0.228 0.403
# 28     1     1     0     1     1  34  47.495  0.511 0.444
# 29     1     1     1     0     0  58  45.856  0.506 0.443
# 30     1     1     1     0     1  82  83.555  0.881 0.534
# 31     1     1     1     1     0  71  90.979  0.678 0.481
# 32     1     1     1     1     1 289 242.373  1.144 0.610


"Factor scores shows tha the most frequent combination (Obs) were
all wrong (271), all correct (289), and only test_4 correct (159). 
Thi high frequency of only test_4 correct (3rd top) shows that the
IRT model is properly reflecting the fact that test_4 was easier than
expected, i.e., much easier than all other tests. If tests were equally
difficult, we would see higher frequency of all correct except one of 
each the other tests. e.g.,(1,0,0,0,0), (0,1,0,0,0), (0,0,1,0,0), and (0,0,0,0,1).

The table goes to an Appendix, but the comment goes to text.


"
hist(factors$score.dat$z1, breaks=10)

#----------------------------------------------------------------
# ALIGN AND RESCALE
# align to start in zero and rescale to fit the original qualification score

df_score.dat <- data.frame(factors$score.dat)

shift <-min(df_score.dat$z1) 
if(shift<0){
  df_score.dat$z1 <-df_score.dat$z1 + abs(shift)
} else if(shift>0){
  df_score.dat$z1 <- df_score.dat$z1 - abs(shift)
}

#rescale to fit the original qualification score between zero and four
df_score.dat$z1 <- scales::rescale(df_score.dat$z1,to=c(0,5))


#----------------------------------------------------------------
#COMPARE z1 score (IRT score) and the qualification score (original)

#Merge data from Qualification Score and IRT Score

df_merged <- left_join(df_consent,df_score.dat,by=c("test1"="test1","test2"="test2","test3"="test3","test4"="test4","test5"="test5"))

#Center (subtract the mean) and Scales (divided by the standard deviation)
qualification_scores <- scale(df_merged$qualification_score, center=TRUE, scale=TRUE)
irt_scores <- scale(df_merged$z1, center=TRUE, scale=TRUE)

score_type <- rep("original",length(qualification_scores))
original_score_list <- cbind(qualification_scores,score_type)
score_type <- rep("adjusted",length(irt_scores))
irt_score_list <- cbind(irt_scores,score_type)

all_score_list <- rbind(original_score_list,irt_score_list)
df_all_scores <- data.frame(all_score_list)
colnames(df_all_scores) <- c("score","type")
df_all_scores$score <- as.numeric(as.character(df_all_scores$score))

df_all_scores %>%
  ggplot(aes(x=score, fill=type)) +
  #geom_histogram(binwidth=0.05, color="darkgrey", fill="lightblue") +
  geom_density(alpha=0.3)+
  theme_minimal()+
  theme(
    legend.position=c(0.85, 0.90),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size=14),
    axis.text.x = element_text(angle = 20, hjust = 1, size=12)
  ) +
  xlab("Scores (centered and scaled)") +
  ylab("Frequency") +
  ggtitle("Qualification score distribution E2") 

"
The chart shows that the adjusted score smoothed the distribution,
but still preserved the two general patterns of concentration on 
high and low medium-to-low scores. The reason for the smoothing is
two-fold: adjusted score is continuous scale and the original 
shifted some very low scores to a low-to-medium score. The latter
corresponds to giving a lower weight to questions that most people
got it correctly This was the case of question 4.
"

#----------------------------------------------------------------
"Merge this with the consent data from E2"

df_score.dat <- data.frame(factors$score.dat)

#Convert to double to be able to join with the fields tes1..5 of df_score.dat
df_score.dat$test1 <- as.factor(df_score.dat$test1)
df_score.dat$test2 <- as.factor(df_score.dat$test2)
df_score.dat$test3 <- as.factor(df_score.dat$test3)
df_score.dat$test4<- as.factor(df_score.dat$test4)
df_score.dat$test5 <- as.factor(df_score.dat$test5)

#LEFT JOIN to associate the new difficulty scores (z1) to the participants.
df_new <- left_join(df_consent,df_score.dat,by=c("test1"="test1","test2"="test2","test3"="test3","test4"="test4","test5"="test5"))

#Note that later on, I aggregate the different qualification and adjusted scores 
#for the same worker_id. This is done in the script load_consent_create_indexes_E2.R

#------------------
#Check if same worker_id has different z-scores
worker_id_list <- unique(df_new$worker_id)

counter <- 0
vector <- character(0) #empty vector
for (id in worker_id_list) {
  irt_score_list <- df_new[df_new$worker_id == id,"qualification_score"]
  different_scores <- unique(irt_score_list)
  if(length(different_scores)>1){
    counter <- counter +1
    vector <- c(vector,id)
  }
}
print(vector)
print(counter)
#42 IDs have more than one score, which means that people took the test twice, 
#which is corroborated by the fact that the id's are composed.
#This happened because these participants removed the cookie that was saved 
#in the local computers after taking the first qualification test.
#This is not a problem. It will generate some variance in the measurements.

#Store in the original file the new difficulty scores (z1) of the participants
write.csv(df_new,"C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data//irt//E2_QualificationTest_IRT.csv")

#Visualizing the results
plot(df_new$years_programming, df_new$z1)
title("Factor Scores by Years of Programming - E2")
plot(df_new$years_programming, df_new$qualification_score)
title("Average Scores by Years of Programming - E2")

cor.test(df_new$years_programming, df_new$z1)
#cor=0.3082745, t = 13.695, df = 1786, p-value < 2.2e-16
cor.test(df_new$years_programming, df_new$qualification_score)
#cor =0.321539 t = 14.351, df = 1786, p-value < 2.2e-16

#Correlation did not change and continue to be significant

#----------------------------------------------------------------

