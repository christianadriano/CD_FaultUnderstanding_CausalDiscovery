"
Investigate answer duration.

- Are there outliers?
- How do professions compare with regards to it by each bug (file name) and code complexity?

"
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)

#Load only Consent data. No data from tasks, only from demographics and qualification test
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E2.R")

df_consent <-
  dplyr::select(df_consent,
                answer_duration,
                file_name,
                complexity,
                is_bugcovering,
                profession
  );

#Check for outliers
df_consent %>%
  ggplot( aes(y=test_duration, x=reorder(profession,1/test_duration))) +
  geom_boxplot()+
  geom_smooth(method = "lm", se=FALSE, fullrange = TRUE, color="steelblue",  linetype="dashed", aes(group=1))+
  stat_summary(fun=mean, geom="point", shape=4, size=3)+
  theme_minimal()+
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size=14),
    axis.text.x = element_text(angle = 20, hjust = 1, size=12)
  ) +
  ylab("Test Duration (minutes)") +
  xlab("Profession") +
  ggtitle("Test Duration across Professions")

"
Replace outliers in testDuration for median values.
This is done for each profession.

A reasonable time for the qualification test in E2 is 25 min, which gives 5 min per question,
which is the average time people took to answer the code inspection tasks. Because,The boxplot shows points that are above 30 min, which is more than 6 min per question

We consider as outliers all data points that are above wiskers in the boxplots. 
These datapoints have values > 3rd quartile + 1.5*interquartile range. 
The interquartile range is the difference between the 2nd and 3rd quartiles
"


profession_list <- as.character(unique(df_consent$profession))

computeMedians <- function(prof){
  median(df_consent[df_consent$profession==prof,]$test_duration)
}
medians_list <- lapply(profession_list, computeMedians)

df_quantiles <- data.frame(matrix(data=c(profession_list,rep(0,24)),ncol=5,nrow = 6, byrow = FALSE)) #initialize with all zeros
colnames(df_quantiles) <- c("profession","median","q2","q3","upper_whisker")

#Quantiles
computeQuantiles <- function(prof){
  quantile(df_consent[df_consent$profession==prof,]$test_duration)
}
quantile_list <- lapply(profession_list,computeQuantiles)

#Proportion below a value
compute_proportion_below <- function(prof,value){
  total <- dim(df_consent[df_consent$profession==prof,])[1]
  below <- dim(df_consent[df_consent$profession==prof & 
                            df_consent$test_duration<value,])[1]
  return(below/total)
}

for(i in c(1:length(profession_list))){
  values <- unlist(quantile_list[i])
  prof <- profession_list[i]
  df_quantiles[df_quantiles$profession==prof,]$q2 <- values[[2]]
  df_quantiles[df_quantiles$profession==prof,]$median <- values[[3]]
  df_quantiles[df_quantiles$profession==prof,]$q3 <- values[[4]]
  inter_quartile <- values[[4]] - values[[2]]
  upper_whisker <- values[[4]] + 1.5 * inter_quartile
  df_quantiles[df_quantiles$profession==prof,]$upper_whisker <- upper_whisker
  df_quantiles[df_quantiles$profession==prof,]$proportion_below_upper_whisker
} 

print(paste0(prof,":",
             compute_proportion_below(prof,upper_whisker))
)
# Profession: Proportion Below Upper Whisker
# "Hobbyist:0.958677685950413"
# "Undergraduate_Student:0.936794582392777"
# "Professional:0.959232613908873"
# "Graduate_Student:0.96113074204947"
# "Other:0.982142857142857"
# "Programmer:0.897959183673469"

"The test_duration outlier removal involved items that were above
90% (Programmer) to 98% (Other) of data points"

"Replace all values that are above the upper whisker for the median time of 
each professional group"

df_consent$profession <- as.factor(df_consent$profession)

for(prof in profession_list){
  upperwhisker <- as.numeric(df_quantiles[df_quantiles$profession==prof,]$upper_whisker)
  median_value <- as.numeric(df_quantiles[df_quantiles$profession==prof,]$median)
  df_consent[df_consent$profession==prof &
               df_consent$test_duration>upperwhisker,]$test_duration <- median_value
}



df_consent %>%
  mutate(text = fct_reorder(profession,test_duration, .desc = TRUE)) %>%
  ggplot( aes(x=test_duration)) +
  geom_density(alpha=0.6, color="darkgrey", fill="lightblue") +
  theme_minimal()+
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size=14),
    axis.text.x = element_text(angle = 20, hjust = 1, size=12)
  ) +
  xlab("Test Duration (minutes)") +
  ylab("Assigned Probability (%)") +
  facet_wrap(~text,nrow=3,ncol=2)+
  ggtitle("Test Duration Across Professions (without outliers)") 

"
Except for Other, all groups present bimodal distributions. 
This might imply that regardless of the profession, we have two groups of  
subjects with respect to the time that they invested in doing the qualification test. 
The more pronounced bimodal figure is among the professionals. 
More time to answer question might reflect thoroughness or cluelessness. 
To evaluate that we need to measure the relation between test duration and the 
qualification score. However, simply computing a correlation or a univariate linear
regression might not work because of the  confounding of Profession or even the other
covariates Age and Years of Programming.
"




df_consent %>%
  ggplot( aes(x=test_duration)) +
  geom_density(alpha=0.6, color="darkgrey", fill="lightblue") +
  theme_minimal()+
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size=14),
    axis.text.x = element_text(angle = 20, hjust = 1, size=12)
  ) +
  xlab("Test Duration (minutes)") +
  ylab("Assigned Probability (%)") +
  ggtitle("Test Duration (without outliers)") 

df_consent %>%
  ggplot( aes(x=test_duration)) +
  geom_histogram(binwidth = 0.5, alpha=0.6, color="darkgrey", fill="lightblue") +
  theme_minimal()+
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size=14),
    axis.text.x = element_text(angle = 20, hjust = 1, size=12)
  ) +
  xlab("Test Duration (minutes)") +
  ylab("Assigned Probability (%)") +
  ggtitle("Test Duration (without outliers)") 

