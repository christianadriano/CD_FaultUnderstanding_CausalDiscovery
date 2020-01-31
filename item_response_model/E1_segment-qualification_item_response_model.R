"
E1 experiment, Item respose model, Programming test qualification

Segmented people in two groups: 
group-1: who got the two easiest items wrong
group-2: who got the two easiest items corect

Removed people who did not get any item correct.

1- If not, did group-1 and group-2 agree on the difficulty of items?
2- Were the items equally discriminative across groups?
"

library(dplyr)
library(tibble)
library(ltm)
library(psych)
library(mirt)

source("C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//load_consent_create_indexes_E1.R")

"Remove the participants for whom we did not take the qualification test" 
df <- df_E1[complete.cases(df_E1[,"qualification_score"]),] #left with 3699 rows

"Replace false for 0(zero) and true for 1(one)"
df$test1_ <-  ifelse(df$test1=="true",1,0)
df$test2_ <-  ifelse(df$test2=="true",1,0)
df$test3_ <-  ifelse(df$test3=="true",1,0)
df$test4_ <-  ifelse(df$test4=="true",1,0)


#------------------------------------------------------

"Count how many people got two items incorrect. Do this for the combination of all 4 items 2 by 2."

#df <-  filter(df,!c(test1_==0 & test2_==0 & test3_==0 & test4_==0))

trbl <- tribble(
    ~pair,~count,
    "1&2",0,
    "1&3",0,
    "1&4",0,
    "2&3",0,
    "2&4",0,
    "3&4",0
    )

#count each combination of two answers incorrect
trbl[trbl$pair=="1&2",2] = dim( filter(df, test1_==0, test2_==0))[1]
trbl[trbl$pair=="1&3",2] = dim( filter(df, test1_==0, test3_==0))[1]
trbl[trbl$pair=="1&4",2] = dim( filter(df, test1_==0, test4_==0))[1]
trbl[trbl$pair=="2&4",2] = dim( filter(df, test2_==0, test4_==0))[1]
trbl[trbl$pair=="3&4",2] = dim( filter(df, test3_==0, test4_==0))[1]
trbl[trbl$pair=="2&3",2] = dim( filter(df, test2_==0, test3_==0))[1]

df_sorted <- trbl[order(-trbl$count),]
df_sorted

#The item pair that most people got incorrect was 1&3 = 2815
#Interestingly, 1 and 3 are not the less difficult items according to 
#the IRT PL2 model. However, 1 and 3 are most discriminative items.


"Run the 2PL model, only difficulty and discrimination"
df_tests <- df %>%  dplyr::select(test1_,test2_,test3_,test4_)
IRT_model_2PL <- ltm(df_tests ~ z1, IRT.param=TRUE)
IRT_model_2PL
#          Dffclt  Dscrmn
# test1_   0.957   4.770
# test2_   1.136  -0.438
# test3_   1.361   1.039
# test4_   0.356   0.188

"Group-1 got 1&3 wrong"
group1 <- filter(df, test1_==0, test3_==0)
group2 <- df[df$test1_!=0 | df$test3_!=0,]

group1 <- group1 %>%  dplyr::select(test1_,test2_,test3_,test4_)
IRT_model_2PL <- ltm(group1 ~ z1, IRT.param=TRUE)
IRT_model_2PL  
#          Dffclt  Dscrmn
# test1_  210.046   0.197
# test2_    0.729   0.396
# test3_  210.046   0.197
# test4_   -0.659  -1.544
group2 <- group2 %>%  dplyr::select(test1_,test2_,test3_,test4_)
IRT_model_2PL <- ltm(group2 ~ z1, IRT.param=TRUE)
IRT_model_2PL  
#         Dffclt   Dscrmn
# test1_  -0.065   15.221
# test2_   0.028   -0.340
# test3_   0.653  -18.590
# test4_   1.970    0.158