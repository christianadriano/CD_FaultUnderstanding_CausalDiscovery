"
qualification_score versus z1

To which professions this adjustment showed more impact?
How to measure this impact? 
  
  I could standardize the scores (force to be between 0 and 1) and see how
many subjects moved up and down. Subjects who would move down, would be 
the ones who got easier questions correct, but the difficult ones wrong.
"
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)

#Load only Consent data. No data from tasks, only from demographics and qualification test
source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data_loaders//load_consent_create_indexes_E2.R")
df_E2_ground<- df_consent

scale_01 <- function(x){(x-min(x))/(max(x)-min(x))}

df_consent$qualification_score_scaled <- scale_01(df_consent$qualification_score)
df_consent$z1_scaled <- scale_01(df_consent$z1)

summary(df_consent$qualification_score_scaled)
summary(df_consent$z1_scaled)

df_consent$decreased_score <- df_consent$qualification_score_scaled>df_consent$z1_scaled

df_test <- df_consent[df_consent$decreased_score==TRUE,]

df_descreased <- data.frame(table(df_test$profession))
# Professional            Programmer              Hobbyist      Graduate_Student 
# 138                    19                   152                    82 
# Undergraduate_Student                 Other 
# 152     37 

colnames(df_descreased) <- c("profession","decreased")

df_original <- data.frame(table(df_consent$profession))
# 417                    49                   484                   283 
# Undergraduate_Student                 Other 
# 443                   112 
colnames(df_original) <- c("profession","original")

joined <- left_join(df_original,df_descreased,by="profession")
joined$proportion <- round((joined$original-joined$decreased)/joined$original, 2)

ggplot(data=joined, aes(x=reorder(profession,proportion), y=proportion))+
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=reorder(proportion,profession)), vjust=1.6, color="white", size=3.5)+
  theme_minimal()+
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 10),
    plot.title = element_text(size=12),
    axis.text.x = element_text(angle = 25, hjust = 1, size=10)
  ) +
  xlab("Profession") +
  ylab("Proportion of subjects") +
  ggtitle("Proportion of subjects whose score decreased after adjustment by IRT model") 

#Difference in reduction (taking the smallest score proportion as baseline)
smallest_score_proportion <- min(joined$proportion)
joined$differences <- round((joined$proportion-smallest_score_proportion) *100,2)

ggplot(data=joined, aes(x=reorder(profession,differences), y=differences))+
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=reorder(differences,profession)), vjust=1.6, color="white", size=3.5)+
  theme_minimal()+
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 10),
    plot.title = element_text(size=12),
    axis.text.x = element_text(angle = 25, hjust = 1, size=10)
  ) +
  xlab("Profession") +
  ylab("Difference relative to Programmers % points") +
  ggtitle("Difference relative to Programmers, which have the argest number of subjects affected")

"
Programmers and Undergraduates were the most affected in numbers. 
The less affected were the Graduate students and Hobbyists, which is surprising.
"

#------------------------------------------------------------------------
#Now I investigate the magnitude of the individual adjustment in score

df_consent$magnitude_adjustment <- df_consent$z1_scaled - df_consent$qualification_score_scaled
"if the magnitud_adjustment is negative, then it means that the score decreased. 
Conversely, if magnitud_adjustment is positive, then the score increased."

#plot the distribution of these magnitutes by profession
df_consent %>%
  ggplot( aes(y=magnitude_adjustment, x=reorder(profession,1-magnitude_adjustment))) +
  geom_boxplot()+
  geom_smooth(method = "lm", se=FALSE, fullrange = TRUE, color="steelblue",  linetype="dashed", aes(group=1))+
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
  #facet_wrap(~text)+
  ylab("Magnitude of score adjustment") +
  xlab("Profession") +
  ggtitle("Magnitude of score adjustment across Professions") 

"
We have two suprising items here.
1- The adjustment was positive for all, except Programmers
2- We can see that although Programmer and Professional were the among the one with 
highest number of people affected by adjustement (Previous barchart), 
the average magnitude of their adjustment were the two top lowest.
"

mean(df_consent[df_consent$profession=="Professional",]$magnitude_adjustment)
mean(df_consent[df_consent$profession=="Programmer",]$magnitude_adjustment)
mean(df_consent[df_consent$profession=="Hobbyist",]$magnitude_adjustment)
mean(df_consent[df_consent$profession=="Other",]$magnitude_adjustment)
mean(df_consent[df_consent$profession=="Undergraduate_Student",]$magnitude_adjustment)
mean(df_consent[df_consent$profession=="Graduate_Student",]$magnitude_adjustment)

"
However, doing an Omnibus test, we could not show any statistically significant distinctions
on these average of adjustments.
"
two.way <- aov(magnitude_adjustment ~ profession,data=df_consent)
summary(two.way)
TukeyHSD(two.way)
