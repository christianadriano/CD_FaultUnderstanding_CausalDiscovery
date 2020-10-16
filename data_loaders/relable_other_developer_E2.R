"
Relabel Others who are software developers and Run Omnibus ANOVA tests to check which groups
are distinct in terms of the covariates of the causal model: qualification_score, 
years of experience and age.

Summary of results:
Looking at the density plots, we can see that there are three groups of professions with 
respect to the qualification_score. 
- Group-1 Others and Undergrads
- Group-2 Graduates and Hobbyists
- Group-3 Professionals and Programmers

However, the omnibus test for age and years of programming showed that a few pairs
did not show statitically significan differences

Age: only  Professional-Hobbyist, p-value=0.9982279

Years Programming: 
 Other-Graduate_Student p-value=0.6470031
 Undergraduate_Student-Graduate_Student p-value=0.7382731
 Other-Hobbyist, p-value=0.5550062
 Undergraduate_Student-Other, p-value=0.0846358
 Programmer-Professional, p-value=0.2886983
 
 This suggests that these pairs are probably more similar in terms of 
years of experience. Students, Programmers, and Hobbyists-Others. We saw thought that
Graduate students are also closer to Others, but not to Hobbyists.

Because the qualification_score is a dependent variable, we will use
causal discovery and inference methods to evaluate the distinctions
across professsions.

Looking at the distribution years of programming,
as we can see that the following groups are similar visually (boxplots 1 and 2).

Doing the t-test for each pair, we confirmed only that one group is 
statistically significant distinct: 
- the Graduate and Undergraduate groups (t = 2.7757, df = 617.83, p-value = 0.005674)

Whereas, for the other two groups we could not reject the null-hypothesis
- the Other and Hobbyist groups (t = 1.3904, df = 187.44, p-value = 0.166)
- the Programmer and Professional groups (t = -1.0093, df = 36.961, p-value = 0.3194)

However With regard to age the pairs within each group are shown to be
statistically significant distinct (p-value<0.05).
Hence, we are not completely sure that we can treat these groups are uniform with respect 
to covariates that might have influence on the qualification score and on the accuracy of
task outcomes..

Implications: So, I would expect that there would be different causal 
models for these three groups.

"

library(farff)
library(readr)
library(ggplot2)
library(dplyr)
#install.packages("hrbrthemes")
library(hrbrthemes)
library(tidyr)

path <- "C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data//"
dataset_E2 <- readARFF(paste0(path, "consent_consolidated_Experiment_2.arff"))
df_E2 <- data.frame(dataset_E2)
df_consent <- data.frame(dataset_E2)
dim(df_consent) #3658

df_consent <- as_tibble(df_consent)
df_consent <- rename(df_consent,profession=experience)

#remove rows without profession information
df_consent <- df_consent[!is.na(df_consent$profession),] #left with 2463
dim(df_consent) #2463

#change to professional so we do no mix up with other developers in the Others category
df_consent[df_consent$profession=="Professional_Developer","profession"] <-"Professional"
#df_consent$profession <- as.factor(df_consent$profession)

#remove rows without test data
dim(df_consent[is.na(df_consent$test1),]) #675 are NA
df_consent <- df_consent[!is.na(df_consent$test1),]
dim(df_consent) #1788 are not NA.

df_others <- df_consent[grep("other",tolower(df_consent$profession)),]
dim(df_others) #161 entries with Other profession who also has test information

"However, we noticed that many people who opted for Other profession are actually programmers.
For some reason, these programmers did not identify themselves as 'Professsional Developer'
This is not good because it distorts the demographics of Others group, which was created
to represent anyone that did not fit in any of the other profession groups. For this reason
we will relable people who are programmers in the Other group as 'Programmer' and the rest as
simply 'Other'. 

We will later on confirm if this group of Programmer is more similar to 'Professional Developer' 
group than with the other groups. For that we will look at their qualification score distribution,
years of experience and age.
" 
pattern <- "it|developer|programmer|computer|tech|technician|software|computer|qa|dba|data"

#create three groups. All other, other developer, other not_developer,
df_consent[(grep(pattern,tolower(df_consent$profession))),"profession"] <- "Programmer"
df_consent[(grep("other",tolower(df_consent$profession))),"profession"] <- "Other"


#Mean qualification score of Other programmers
hist(df_consent[df_consent$profession=="Programmer",]$qualification_score)
hist(df_consent[df_consent$profession=="Other",]$qualification_score)

t.test(
      df_consent[df_consent$profession=="Programmer","qualification_score"],
      df_consent[df_consent$profession=="Other","qualification_score"],
      alternative = c("two.sided")
)
# t = 6.5172, df = 219.06, p-value = 4.856e-10
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.7241891 1.3520609
# sample estimates:
#   mean of x mean of y 
# 2.960000  1.921875 

t.test(
  df_consent[df_consent$profession=="Programmer","qualification_score"],
  df_consent[df_consent$profession=="Professional","qualification_score"],
  alternative = c("two.sided")
)
# t = -0.91952, df = 40.459, p-value = 0.3633
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.9155798  0.3428380
# sample estimates:
#   mean of x mean of y 
# 2.694444  2.980815

p <- df_consent %>%
  ggplot( aes(x=qualification_score, fill=profession)) +
  geom_density( color="#e9ecef", alpha=0.6, position = 'identity') +
  theme_ipsum() +
  labs(fill="")
p




#--------------------------------
#ANOVA OMNIBUS TEST

#Are Profession Groups distinct in terms of age and years of programming?

#AGE
#Remove outliers (age has an outlier of 100 years old)
df_consent[df_consent$profession=="Undergraduate_Student" &
                 df_consent$age==100,]$age <- median(as.numeric(df_consent[
                   df_consent$profession=="Undergraduate_Student",]$age))
df_consent[df_consent$profession=="Professional" &
                 df_consent$age==16,]$age <- median(df_consent[
                   df_consent$profession=="Professional",]$age)
df_consent[df_consent$profession=="Graduate_Student" &
                 df_consent$age==2,]$age <- median(df_consent[
                   df_consent$profession=="Graduate_Student",]$age)


two.way <- aov(age ~ profession,data=df_consent)
summary(two.way)
TukeyHSD(two.way)
"The only pair that did not rejected the null-hypothesis 
was Professional-Hobbyist, p-value=0.9980065"

two.way <- aov(years_programming ~ profession,data=df_consent)
summary(two.way)
TukeyHSD(two.way)
#The only pairs that did not rejected the null-hypothesis were:
# Other-Graduate_Student p-value=0.6470031
# Undergraduate_Student-Graduate_Student p-value=0.7382731
# Other-Hobbyist, p-value=0.5550062
# Undergraduate_Student-Other, p-value=0.0846358
# Programmer-Professional, p-value=0.2886983
"This suggests that these pairs are probably more similar in terms of 
years of experience. Students, Programmers, and Hobbyists-Others. We saw thought that
Graduate students are also closer to Others, but not to Hobbyists."

"
The test for differences in qualification_score is more complex because qualifcation score
is dependent variable, i.e., is possible affected by age, years of experience and profession.
These covariates might interact with each other and confound the effect qualification score.
Hence, we need to hypothesize different linear models
lm_1: qualification_score = profession + age + years_programming
lm_2: qualification_score = profession + age + years_programming + age*years_programming

The best instruments for this are to do Causal Discovery methods and Multi-Level Models.
Which we will look at next (see file )

"
#---------------------------------------------

# Multiple small plots
#install.packages("viridis")
library(viridis)
library(forcats)
library(gridExtra)

#QUALIFICATION SCORE

df_consent %>%
  mutate(text = fct_reorder(profession,qualification_score, .desc = TRUE)) %>%
  ggplot( aes(x=qualification_score)) +
  geom_density(alpha=0.6, binwidth = 1,color="darkgrey", fill="lightblue") +
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
  xlab("Qualification Score") +
  ylab("Assigned Probability (%)") +
  facet_wrap(~text,nrow=3,ncol=2)+
  ggtitle("Qualification Score Across Professions") -> gg
gg



#YEARS PROGRAMMING
df_consent %>%
  ggplot( aes(y=years_programming, x=reorder(profession,1/age))) +
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
  #facet_wrap(~text)+
  ylab("Years of Programming") +
  xlab("Profession") +
  ylim(-5,40)+
  ggtitle("Years of Programming across Professions") -> gg1
gg1



#AGE
df_consent %>%
  ggplot( aes(y=age, x=reorder(profession,1/age))) +
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
  #facet_wrap(~text)+
  ylab("Age") +
  xlab("Profession") +
  ylim(10,75)+
  ggtitle("Age across Professions") -> gg
gg


#-----------------------------------
#T-Tests to compare within groups

#Hobbyist and Others
t.test(df_consent[df_consent$profession=="Hobbyist","age"],df_consent[df_consent$profession=="Other","age"])
#t = -2.4409, df = 164.14, p-value = 0.01571 SIGNIFICANT
t.test(df_consent[df_consent$profession=="Hobbyist","years_programming"],df_consent[df_consent$profession=="Other","years_programming"])
#t = 1.3904, df = 187.44, p-value = 0.166 NOT SIGNIFICANT

#Professional and Programmer
t.test(df_consent[df_consent$profession=="Professional","age"],df_consent[df_consent$profession=="Programmer","age"])
#t = -2.8632, df = 36.568, p-value = 0.006903 SIGNIFICANT
t.test(df_consent[df_consent$profession=="Professional","years_programming"],df_consent[df_consent$profession=="Programmer","years_programming"])
#t = -1.0093, df = 36.961, p-value = 0.3194 NOT SIGNIFICANT

#Graduate and Undergraduate Student
t.test(df_consent[df_consent$profession=="Graduate_Student","age"],df_consent[df_consent$profession=="Undergraduate_Student","age"])
#t = 5.1057, df = 666, p-value = 4.306e-07 SIGNIFICANT
t.test(df_consent[df_consent$profession=="Graduate_Student","years_programming"],df_consent[df_consent$profession=="Undergraduate_Student","years_programming"])
#t = 2.7757, df = 617.83, p-value = 0.005674 SIGNIFICANT

