"
Investigate test duration.

Are there outliers?
How do professions compare with regards to it?

"
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)

#Load only Consent data. No data from tasks, only from demographics and qualification test
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E2.R")

df_consent <-
  dplyr::select(df_consent,
                years_programming,
                z1,
                testDuration_minutes,
                age,
                profession
  );

#Check for outliers
df_consent %>%
  ggplot( aes(y=testDuration_minutes, x=reorder(profession,1/testDuration_minutes))) +
  geom_boxplot()+
  stat_summary(fun=mean, geom="point", shape=4, size=3)+
  theme_ipsum_pub()+
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 10),
    panel.grid=element_blank(),
    plot.title = element_text(size=12),
    axis.text.x = element_text(angle = 25, hjust = 1, size=10)
  ) +
  ylab("Test Duration (minutes)") +
  xlab("Profession") +
  ylim(10,35)+
  ggtitle("Test Duration across Professions") -> gg
gg

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
  median(df_consent[df_consent$profession==prof,]$testDuration_minutes)
}
medians_list <- lapply(profession_list, computeMedians)

df_quantiles <- data.frame(matrix(data=c(profession_list,rep(0,24)),ncol=5,nrow = 6, byrow = FALSE)) #initialize with all zeros
colnames(df_quantiles) <- c("profession","median","q2","q3","upper_wisker")

#Quantiles
computeQuantiles <- function(prof){
  quantile(df_consent[df_consent$profession==prof,]$testDuration_minutes)
}
quantile_list <- lapply(profession_list,computeQuantiles)

for(i in c(1:length(profession_list))){
  values <- unlist(quantile_list[i])
  prof <- profession_list[i]
  df_quantiles[df_quantiles$profession==prof,]$q2 <- values[[2]]
  df_quantiles[df_quantiles$profession==prof,]$median <- values[[3]]
  df_quantiles[df_quantiles$profession==prof,]$q3 <- values[[4]]
  inter_quartile <- values[[4]] - values[[2]]
  df_quantiles[df_quantiles$profession==prof,]$upper_wisker <- values[[4]] + 1.5 * inter_quartile
} 
  

#Replace all values that are above 30 min to the median of each professional group

df_consent$profession <- as.factor(df_consent$profession)

for(prof in profession_list){
  upperwisker <- as.numeric(df_quantiles[df_quantiles$profession==prof,]$upper_wisker)
  median_value <- as.numeric(df_quantiles[df_quantiles$profession==prof,]$median)
  df_consent[df_consent$profession==prof &
             df_consent$testDuration_minutes>upperwisker,]$testDuration_minutes <- median_value
}



df_consent %>%
  mutate(text = fct_reorder(profession,testDuration_minutes, .desc = TRUE)) %>%
  ggplot( aes(x=testDuration_minutes)) +
  geom_density(alpha=0.6, color="darkgrey", fill="lightblue") +
  #scale_fill_viridis(discrete=TRUE, option="E")+
  #scale_color_viridis(discrete=TRUE, option = "E")+
  theme_ipsum_pub()+
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 10),
    panel.grid=element_blank(),
    plot.title = element_text(size=12)
  ) +
  xlab("Test Duration (minutes)") +
  ylab("Assigned Probability (%)") +
  facet_wrap(~text,nrow=3,ncol=2)+
  ggtitle("Test Duration Across Professions") -> gg
gg
