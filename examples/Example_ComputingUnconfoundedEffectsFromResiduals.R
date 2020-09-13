library(dplyr)
data(swiss)
help(swiss)
cor(swiss$Fertility,swiss$Education)
#[1] -0.6637889

#Stratify and condition onf Agriculture
swiss <- swiss %>%
  group_by(cut(Agriculture,breaks=3)) %>%
  mutate(Fert.resid = Fertility - mean(Fertility),
         Ed.resid = Education - mean(Education))

cor(swiss$Fert.resid,swiss$Ed.resid)
#[1] -0.5560316