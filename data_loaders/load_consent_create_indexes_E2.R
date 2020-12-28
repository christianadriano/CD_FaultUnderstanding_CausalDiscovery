"
Load CONSENT data from E2 and create indexes for: profession, qualification_score, file_name, country, etc.

Data sources:
consent_consolidated_Experiment_2.arff (produced by project DW_Microtasks)
E2_QualificationTest_IRT.csv (produced by scripts in CausalModel_FaultUnderstanding item_response_model)


"
library(farff)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)

#--------------------------
"LOAD FILES"

path <- "C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//"
dataset_E2 <- readARFF(paste0(path,"//data//","consent_consolidated_Experiment_2.arff"))
df_consent <- data.frame(dataset_E2)
dim(df_consent) 

#------------------------
"MISSING DATA"
#remove rows without profession information
df_consent <- df_consent[!is.na(df_consent$profession),] #left with 2463
dim(df_consent) #2463

#Filter-out rows without test data
dim(df_consent[is.na(df_consent$test1),]) #1870 are NA
df_consent <- df_consent[!is.na(df_consent$test1),]
dim(df_consent) #1788 are not NA.

#------------------------
"PROFESSION"
#relable professional_developer as professional
df_consent[df_consent$profession=="Professional_Developer",]$profession <- "Professional"

#relable Others who are programmers
pattern <- "it|developer|programmer|computer|tech|technician|software|computer|qa|dba|data|physicist|systems|analyst|engineer"
df_consent[(grep(pattern,tolower(df_consent$profession))),"profession"] <- "Programmer"
df_consent[(grep("other",tolower(df_consent$profession))),"profession"] <- "Other"


#Convert profession from factor to character 
#df_consent$profession <- as.character(df_consent$experience)
#Replaces 'Other...something' for only 'Other'
#df_consent[grep("Other",df_consent$profession),"profession"] <- "Other"
#Transform profession as factor
df_consent$profession <- factor(df_consent$profession, 
                           levels = c("Professional","Programmer", "Hobbyist",
                                      "Graduate_Student","Undergraduate_Student",
                                      "Other")
)
df_consent$profession_id <- factor(df_consent$profession,
                              levels=levels(df_consent$profession),
                              labels=c(1:6)
)

#------------------------
"QUALIFICATION_SCORE"

#Transform profession as factor again

df_consent$qualification_score_label<- factor(df_consent$qualification_score, 
                                          levels = c(5:0),
                                          labels = c("100%","80%","60%","40%","20%","0%")
                                          )
df_consent$qualification_score_id <- factor(df_consent$qualification_score_label,
                              levels=levels(df_consent$qualification_score_label),
                              labels=c(5:0)
)

#--------------------------
"ITEM RESPONSE MODEL SCORES
Merge Score factors computed through IRT Model fitting"
df_irt <- read.csv(paste0(path,"//data//irt//","E2_QualificationTest_IRT.csv"))
df_irt <-  dplyr::select(df_irt, worker_id,file_name,z1) #need file_name, because a few workers have more than one score.
df_irt$worker_id <- as.factor(df_irt$worker_id) #convert to factor, so I can join with worker_id column
df_consent$worker_id <- as.factor(df_consent$worker_id) #convert to factor, so I can join with worker_id column
#only joins with people who qualified (qualification_score>=3), because only these are present in the task execution logs
df_consent <- left_join(x=df_consent,y=df_irt,keep=FALSE, by=c("worker_id"="worker_id","file_name"="file_name"))#,"file_name"="file_name"))
dim(df_consent) 
df_consent <- rename(df_consent,adjusted_score=z1)

#------------------------
"FILE_NAME"

#df_consent <- df_consent[df_consent$file_name!="null",]

df_consent$file_name<- factor(df_consent$file_name, 
                         levels = c("HIT01_8","HIT02_24","HIT03_6","HIT04_7",
                                    "HIT05_35","HIT06_51","HIT07_33","HIT08_54")
                        )
df_consent$file_name_id <- factor(df_consent$file_name,
                             levels=levels(df_consent$file_name),
                             labels=c(1:8)
)

#------------------------
"GENDER"

df_consent$gender<- factor(df_consent$gender, 
                      levels = c("Female","Male","Prefer_not_to_tell","Other")
)

df_consent$gender_id<- factor(df_consent$gender, 
                         levels=levels(df_consent$gender),
                         labels = c(1:4)
)

#----------------------------------
"AGE and YEARS OF PROGRAMMING"
#Deal with wrong input (age == 100 year)
median_age_undergrad <- median(df_consent[df_consent$profession=="Undergraduate_Student",]$age)
df_consent[df_consent$age==100,]$age <- median_age_undergrad

#Deal with age==yoe (wrong inputation by participants)
worker_id_list <- df_consent[df_consent$age<=df_consent$years_programming,]$worker_id
#View(df_consent[df_consent$worker_id %in% worker_id_list,c("worker_id","profession","age","years_programming")])
# worker_id                         profession            age years_programming
# 817EE-1e2C78-7:129eE4a-1E-24-3_3	Hobbyist	            32	32
# 544cg-9e1A107_3	                  Undergraduate_Student	21	21
# 828cC0C9E-5-7-4_3	                Graduate_Student	    2	  2
# 1598AG0g-8e-9-32_3	              Professional	        25	25
# 110Gi-9C-7a4-12_3	                Graduate_Student	    18	18

#Solution will be to use median age and median years of programming for all.
median_age_graduate <- median(df_consent[df_consent$profession=="Graduate_Student",]$age)
median_age_hobbyist <- median(df_consent[df_consent$profession=="Hobbyist",]$age)
median_age_professional <- median(df_consent[df_consent$profession=="Professional",]$age)

median_yoe_undergrad <- median(df_consent[df_consent$profession=="Undergraduate_Student",]$years_programming)
median_yoe_graduate <- median(df_consent[df_consent$profession=="Graduate_Student",]$years_programming)
median_yoe_hobbyist <- median(df_consent[df_consent$profession=="Graduate_Student",]$years_programming)
median_yoe_professional <- median(df_consent[df_consent$profession=="Professional",]$years_programming)

df_consent[df_consent$worker_id=="817EE-1e2C78-7:129eE4a-1E-24-3_3","age"] <- median_age_hobbyist
df_consent[df_consent$worker_id=="817EE-1e2C78-7:129eE4a-1E-24-3_3","years_programming"] <- median_yoe_hobbyist

df_consent[df_consent$worker_id=="544cg-9e1A107_3","age"] <- median_age_undergrad
df_consent[df_consent$worker_id=="544cg-9e1A107_3","years_programming"] <- median_yoe_undergrad

df_consent[df_consent$worker_id=="828cC0C9E-5-7-4_3","age"] <- median_age_graduate
df_consent[df_consent$worker_id=="828cC0C9E-5-7-4_3","years_programming"] <- median_yoe_graduate

df_consent[df_consent$worker_id=="1598AG0g-8e-9-32_3","age"] <- median_age_professional
df_consent[df_consent$worker_id=="1598AG0g-8e-9-32_3","years_programming"] <- median_yoe_professional

df_consent[df_consent$worker_id=="110Gi-9C-7a4-12_3","age"] <- median_age_graduate
df_consent[df_consent$worker_id=="110Gi-9C-7a4-12_3","years_programming"] <- median_yoe_graduate

#------------------------
"COUNTRY"


df_consent$country <- unlist(lapply(df_consent$country, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}))

df_consent$country <- gsub("\\bUNITED STATES OF AMERICA\\b","US",df_consent$country)
df_consent$country <- gsub("\\bUNITED STATES\\b","US",df_consent$country)
df_consent$country <- gsub("\\bUNITES STATES\\b","US",df_consent$country)
df_consent$country <- gsub("\\bUNITEDE STATES\\b","US",df_consent$country)
df_consent$country <- gsub("\\bUNITED STAETS\\b","US",df_consent$country)
df_consent$country <- gsub("\\bUNITED\\b","US",df_consent$country)
df_consent$country <- gsub("\\bUNIT\\b","US",df_consent$country)
df_consent$country <- gsub("\\bAMERICA\\b","US",df_consent$country)
df_consent$country <- gsub("LOS ANGELES","US",df_consent$country)
df_consent$country <- gsub("ILLINOIS","US",df_consent$country)
df_consent$country <- gsub("WISCONSIN","US",df_consent$country)
df_consent$country <- gsub("RIVERSIDE","US",df_consent$country)
df_consent$country <- gsub("SAN JOSE CA","US",df_consent$country)
df_consent$country <- gsub("SAN DIEGO","US",df_consent$country)
df_consent$country <- gsub("LOUISIANA","US",df_consent$country)
df_consent$country <- gsub("CALIFORNIA","US",df_consent$country)
df_consent$country <- gsub("ARIZONA","US",df_consent$country)
df_consent$country <- gsub("OHIO","US",df_consent$country)
df_consent$country <- gsub("MARYLAND","US",df_consent$country)
df_consent$country <- gsub("CINCINNATI","US",df_consent$country)
df_consent$country <- gsub("\\bIL\\b","US",df_consent$country)
df_consent$country <- gsub("\\bIN\\b","US",df_consent$country)

df_consent$country <- gsub("U\\.S\\.A\\.","US",df_consent$country)
df_consent$country <- gsub("U\\.S\\.","US",df_consent$country)
df_consent$country <- gsub("U\\.S","US",df_consent$country)
df_consent$country <- gsub("\\bUSA\\b","US",df_consent$country)
df_consent$country <- gsub("GG","OTHER",df_consent$country)
df_consent$country <- gsub("NO","OTHER",df_consent$country)
df_consent$country <- gsub("GURGAON, INDIA","INDIA",df_consent$country)
df_consent$country <- gsub("INDIAN","INDIA",df_consent$country)
df_consent$country <- gsub("MADURAI","INDIA",df_consent$country)
df_consent$country <- gsub("SRILANKA","SRI LANKA",df_consent$country)
df_consent$country <- gsub("ENGLAND","UK",df_consent$country)

df_tb <-  data.frame(table(df_consent$country))
colnames(df_tb) <- c("country","participants")
tribble_cnty <- df_tb %>% group_by(participants)
tribble_cnty <- tribble_cnty %>% summarise(countries_by_participants = n())
tribble_cnty$participants_labels <- as.factor(tribble_cnty$participants)

# barplot(tribble_cnty$countries_by_participants,
#         names.arg = tribble_cnty$participants,
#         xlab="Countries with N participants",
#         ylab="Number of countries",
#         main="Countries by number of participants - E2",
#         ymin=0, ymax=15
#         )
# 
# ggplot(data=tribble_cnty, aes(x=participants_labels, y=countries_by_participants)) +
#   geom_bar(stat="identity", fill="lightgray")+
#   geom_text(aes(label=countries_by_participants), vjust=-0.3, color="black", size=3.0)+
#   theme_minimal()+
#   theme(plot.title = element_text("Helvetica-Narrow", face="plain", colour="black", size=10))+
#   theme(axis.title.x = element_text("Helvetica-Narrow", face="plain", colour="black", size=8))+
#   theme(axis.title.y = element_text("Helvetica-Narrow", face="plain", colour="black", size=8))+
#   theme(axis.text.x  = element_text("Helvetica-Narrow", face="plain", colour="black", size=8))+
#   theme(axis.text.y  = element_text("Helvetica-Narrow", face="plain", colour="black", size=8))+
#   xlab("Number of participants")+
#   ylab("Number of countries")+
#   ggtitle("Countries by number of participants - E2")

df_consent$country <- unlist(lapply(df_consent$country, 
                               function(v) {
                                 if(v %in%  c("US","INDIA")) return(v)
                                 else return("OTHER")
                               }))

df_consent$country_labels<- factor(df_consent$country,
                              levels = c("US","INDIA","OTHER")
)

df_consent$country_id<- factor(df_consent$country_labels, 
                          levels=levels(df_consent$country_labels),
                          labels = c(1:3)
)

#------------------------
#DURATION of TEST
#Convert to minutes
df_consent$test_duration <- df_consent$test_duration/(1000*60)

"
Replace outliers in test_duration for median values.
This is done for each profession.

A reasonable time for the qualification test in E2 is 25 min, which gives 5 min per question,
which is the average time people took to answer the code inspection tasks. Because,The boxplot shows points that are above 30 min, which is more than 6 min per question

We consider as outliers all data points that are above wiskers in the boxplots. 
These datapoints have values > 3rd quartile + 1.5*interquartile range. 
The interquartile range is the difference between the 2nd and 3rd quartiles
"

profession_list <- as.character(unique(df_consent$profession))

computeMedians <- function(prof){
  median(df_consent[df_consent$profession==prof,]$test_duration)
}
medians_list <- lapply(profession_list, computeMedians)

df_quantiles <- data.frame(matrix(data=c(profession_list,rep(0,24)),ncol=5,nrow = 6, byrow = FALSE)) #initialize with all zeros
colnames(df_quantiles) <- c("profession","median","q2","q3","upper_wisker")

#Quantiles
computeQuantiles <- function(prof){
  quantile(df_consent[df_consent$profession==prof,]$test_duration)
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
               df_consent$test_duration>upperwisker,]$test_duration <- median_value
}
"FAST TEST ANSWER MEMBERSHIP
Merge the membership column that tells whether a worker is part of the fast or slow test takers.
This column was produced by building a Gaussian Mixture model.
"
df_fastMembership <- read.csv(paste0(path,"data//mixture_model//","consent_with_testDuration_fastMembership.csv"))
df_fastMembership <- 
  dplyr::select(df_fastMembership,
                worker_id,
                file_name,
                profession,
                testDuration_fastMembership,
                is_fast
  );
df_consent <- left_join(df_consent,df_fastMembership,by=c("worker_id","file_name","profession"),
                        copy= FALSE)


df_consent$is_fast <- df_consent$testDuration_fastMembership>=0.5


print(paste0("Loaded ",dim(df_consent)[1], " rows."," Results are in df_consent"))

#---------------------
#END
#---------------------

