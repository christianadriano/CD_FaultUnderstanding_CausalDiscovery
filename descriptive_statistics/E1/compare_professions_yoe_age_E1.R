"
E1 Compare programmers by Years of Programming Experience and Age

"

library(ggplot2)
library(hrbrthemes)
library(tidyr)
library(tidyverse)
library(dplyr)

#Load only Consent data. No data from tasks, only from demographics and qualification test
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E1.R")

df_consent <- load_consent_create_indexes(load_is_student=1)
#Remove NA's
df_E1 <- df_consent[complete.cases(df_consent[,c("years_programming","age")]),]

df_E1 %>%
  mutate(text = fct_reorder(profession,years_programming, .desc = TRUE)) %>%
  ggplot( aes(x=years_programming)) +
  geom_density(alpha=0.6, color="darkgrey", fill="lightblue") +
  theme_minimal()+
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size=14),
    axis.text.x = element_text(hjust = 1, size=12)
  ) +
  xlab("Years of Programming") +
  ylab("Density (%)") +
  facet_wrap(~text,nrow=3,ncol=2)+
  ggtitle("E1: Years of Programming across Professions") 

"Students show a more concentrated distribution"

#Boxplots
df_E1 %>%
  mutate(text = fct_reorder(profession,years_programming, .desc = TRUE)) %>%
  ggplot( aes(y=years_programming, x=text)) +
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
  ylab("Years of Programming") +
  ggtitle("E1: Years of Programming across Professions") 

"As expected, non-students have higher programming experience, which 
contributes to a wider ranges (size of inter-quartiles, i.e., boxes). 

Concerning outliers, non-students also have more outliers, which 
is expected for a group that is more diverse."

#----------------------------------------------------------------
#AGE

df_E1 %>%
  mutate(text = fct_reorder(profession,age, .desc = TRUE)) %>%
  ggplot( aes(x=age)) +
  geom_density(alpha=0.6, color="darkgrey", fill="lightblue") +
  theme_minimal()+
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size=14),
    axis.text.x = element_text(hjust = 1, size=12)
  ) +
  xlab("Age (years)") +
  ylab("Density (%)") +
  facet_wrap(~text,nrow=3,ncol=2)+
  ggtitle("E1: Age of Participants across Professions") 

"Very different distribution shapes. Studets are more concetrated and
more lepticurtic, whereas non-students are more spread (platicurtic) and
have peak (highest density) above the students."

#Boxplots
df_E1 %>%
  ggplot( aes(y=age, x=reorder(profession,1/age))) +
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
  ylab("Age (years)") +
  ggtitle("E1: Age of Participants across Professions") 
"
Besides show no overlap between dsitributions, the chart
shows that non-students also present more outliers w.r.t. 
age.
"