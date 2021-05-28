"
E1 Compare adjusted_score and qualification_score (original)

1- How adjusted_score and qualification_score are distributed 
- How many people increased their score relative to their peers? 
- How many people decreased? 
- Who were these people who increased and decreased (distribution w.r.t. original score)?

"
library(ggplot2)


"LOAD FILES"
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E1.R")
df_consent <- load_consent_create_indexes();

#----------------------------------------------------------------
# 1- How adjusted_score and qualification_score are distributed 

#Merge data from Qualification Score and IRT Score

df_score <- data.frame(as.matrix(factors$score.dat)) 
head(df_score)

df_merged <- left_join(df,df_score,by=c("test1_"="test1_","test2_"="test2_","test3_"="test3_","test4_"="test4_"))

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
high and low medium-to-low scores. 

However, it also increased the frequency of the lowest score and
highest score groups. The final distribution has more oa an
exponential shape, while the original one had a right skewed Gaussian
shape.


The reason for the smoothing is two-fold: adjusted score is continuous scale and the original 
shifted some very low scores to a low-to-medium score. The latter
corresponds to giving a lower weight to questions that most people
got it correctly This was the case of question 4.
"
#----------------------------------------------------------------

