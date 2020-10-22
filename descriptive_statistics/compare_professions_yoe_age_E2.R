"
Compare programmers by Years of Programming Experience and Age

"

library(ggplot2)
library(hrbrthemes)
library(tidyr)
library(tidyverse)
library(dplyr)

#Load only Consent data. No data from tasks, only from demographics and qualification test
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E2.R")

df_consent %>%
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
  ggtitle("Years of Programming across Professions") 

"We can see in the density plots that groups show similar shapes of distribution. 
I would say that Programmer is a more dispersed version of professional,
and the same of Hobbyists with relation to Graduates and Others to Undergrads"

#Boxplots
df_consent %>%
  ggplot( aes(y=years_programming, x=reorder(profession,1/years_programming))) +
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
  ggtitle("Years of Programming across Professions") 

"As expected, the more senior groups have higher programming experience, which 
contributes to a wider ranges (size of inter-quartiles, i.e., boxes). 

Concerning outliers, except for Programmers, all other professions have a similar 
outliers counts. The only distinction is the outliers for Students are below 
20 years of experience, whereas the "Other" go up to 35 years. This makes sense, 
because students are supposed to be among the youngest  study cohorts."

#----------------------------------------------------------------
#AGE

df_consent %>%
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
  ggtitle("Age of Participants across Professions") 

"We can see in the density plots that groups show similar shapes of distribution. 
I would say that Programmer is a more dispersed version of professional,
and the same of Hobbyists with relation to Graduates and Others to Undergrads"

#Boxplots
df_consent %>%
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
  ggtitle("Age of Participants across Professions") 