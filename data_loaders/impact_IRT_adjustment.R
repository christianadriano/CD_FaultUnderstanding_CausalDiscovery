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
library(hrbrthemes)
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

df_descrease <- data.frame(table(df_test$profession))
# Professional            Programmer              Hobbyist      Graduate_Student 
# 138                    19                   152                    82 
# Undergraduate_Student                 Other 
# 152     37 

colnames(df_descrease) <- c("profession","decreased")

df_original <- data.frame(table(df_consent$profession))
# 417                    49                   484                   283 
# Undergraduate_Student                 Other 
# 443                   112 
colnames(df_original) <- c("profession","original")

joined <- left_join(df_original,df_descrease,by="profession")
joined$proportion <- (joined$original-joined$decreased)/joined$original

ggplot(data=joined, aes(x=reorder(profession,proportion), y=proportion))+
  geom_bar(stat="identity")+
#  theme_ipsum_pub()+
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 10),
    panel.grid=element_blank(),
    plot.title = element_text(size=12),
    axis.text.x = element_text(angle = 25, hjust = 1, size=10)
  ) +
  xlab("Profession") +
  ylab("Reduction in score") +
  ggtitle("Proportion that reduced their score after adjustment by IRT model") 

#Difference in reduction (taking the smallest score proportion as baseline)
smallest_score_proportion <- min(joined$proportion)
joined$differences <- round((joined$proportion-smallest_score_proportion) *100,2)

ggplot(data=joined, aes(x=reorder(profession,differences), y=differences))+
  geom_bar(stat="identity", fill="darkgrey")+
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
Still need to compute the magnitude to the decrease.

"
