"
How do professions (including Programmers) compare with respect to 
Adjusted_Score (Z1 from IRT) and Qualification_Score?

Visually (Density Plots and Boxplots)
Statistical distinct (ANOVA)

TODO:
Later on, when I build the causal model with the inspection tasks,
I can compare the how well the score using z1 versus qualification_score can 
predict the accuracy of the inspection task.

We learned that the causal discovery methods produced the sames graphs 
when we either used z1 or qualification_score. Hence, the choice for adjustment
will not affect how we do matching and inference, but might still affect the 
power of the causal models to make predictions and explain effects.

"

library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tidyr)
library(tidyverse)

#Load only Consent data. No data from tasks, only from demographics and qualification test
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E2.R")
df_E2_ground<- df_consent

"VISUAL COMPARISON"

#QUALIFICATION SCORE
#Density Plots
df_consent %>%
  mutate(text = fct_reorder(profession,qualification_score, .desc = TRUE)) %>%
  ggplot( aes(x=qualification_score)) +
  geom_density(alpha=0.6, color="darkgrey", fill="lightblue") +
  theme_minimal()+
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size=14),
    axis.text.x = element_text(angle = 20, hjust = 1, size=12)
  ) +
  xlab("Qualification Score") +
  ylab("Assigned Probability (%)") +
  facet_wrap(~text,nrow=3,ncol=2)+
  ggtitle("Qualification Score across Professions") 

#Boxplots
df_consent %>%
  mutate(text = fct_reorder(profession,qualification_score, .desc = TRUE)) %>%
  ggplot( aes(y=qualification_score, x=text)) +
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
  xlab("Profession") +
  ylab("Qualification score") +
  ggtitle("Qualification Score across Professions") 

#Adjusted Score (z1 from IRT)
#Density Plots
df_consent %>%
  mutate(text = fct_reorder(profession,z1, .desc = TRUE)) %>%
  ggplot( aes(x=z1)) +
  geom_density(alpha=0.6, color="darkgrey", fill="lightblue") +
  theme_minimal()+
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size=14),
    axis.text.x = element_text(angle = 20, hjust = 1, size=12)
  ) +
  xlab("Adjusted Score (z1 IRT)") +
  ylab("Assigned Probability (%)") +
  facet_wrap(~text,nrow=3,ncol=2)+
  ggtitle("Adjusted Score (z1) across Professions") 

#Boxplots
df_consent %>%
  ggplot( aes(y=z1, x=reorder(profession,1-z1))) +
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
  xlab("Profession") +
  ylab("Adjusted Score (z1)") +
  ggtitle("Adjusted Score (z1) across Professions") 

#----------------------
"Looking at the boxplots we should not expect to have many statistically significant
differences among groups.

"
#ANOVA and TUKEY Honest Significant Differences
#Qualification Score
two.way <- aov(qualification_score ~ profession,data=df_consent)
summary(two.way)
TukeyHSD(two.way)
"
Professionals are statistically significant distinct to all professions others, 
except Programmers, which reforces the idea that Professional and Programmers are similar group.
Programmers are also distinct to all professions except two: professionals and undergraduates.
Meanwhile, studens, other, and hobbyists are not statistically significant distinct.
"
#Adjusted Score z1
two.way <- aov(z1 ~ profession,data=df_consent)
summary(two.way)
TukeyHSD(two.way)

"
Professionals are statistically significant distinct to all professions others, 
except Programmers, which reforces the idea that Professional and Programmers are similar group.
However, Programmers are only distinct to Other.
Meanwhile, students, others,and hobbyists are not statistically significant distinct.

Hence, z1 makes Programmers more similar to all professions (except Others), which is
another reassurance that Programmers should not be part of the Other group.
"






