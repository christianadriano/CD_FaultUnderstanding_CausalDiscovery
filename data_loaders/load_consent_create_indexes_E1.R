"
Load data from E1 and execute the following data wrangling procedures:
- create indexes for various columns 
- merge IRT data
- deal with inputation error
- missing data
- label programmers as fast w.r.t. to their testDuration_Membership
"

library(farff)
library(ggplot2)
library(dplyr)

"--------------------------------------------------------------------
About the load function
is_student =1 adds a column with the classification of is_student_E1.csv
default = 1
"
load_consent_create_indexes_E1 <- function(load_is_student=1){
  
  #--------------------------
  "LOAD FILES"
  path <- "C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data//"
  dataset_E1 <- readARFF(paste0(path,"consent_consolidated_Experiment_1.arff"))
  df_consent <- data.frame(dataset_E1)
  dim(df_consent) #4776   16
  
  
  #------------------------
  "MISSING DATA"
  
  #Filter-out rows without test data, because there is no information about these 
  #as in the E1 the demographics are collected at the end of the task
  dim(df_consent[is.na(df_consent$test1),]) #1077 are NA
  df_consent <- df_consent[complete.cases(df_consent[,"qualification_score"]),]
  dim(df_consent) #3699 are not NA.
  
  #-----------------------
  "INPUTATION ERROR"
  
  #Filter-out 3 rows that have demographics data but did not pass the test (qualification score<2)
  #This should not have happened in normal execution, because demographics were collected only 
  #at the end of the experiment. 
  df_consent <- df_consent[!(df_consent$qualification_score<2 & !is.na(df_consent$years_programming)),]
  
  #-----------------------
  "QUALIFICATION_SCORE"
  
  "I cannot transform qualification score as a factor anymore for two reasons:
    - it is reasonable to assume a continuous scale 
    - there will be more than the integer values, because some workers will
    have averaged values that result from the fact that they took more 
    than one type of test (there were four types)
  "
  
  # Averaging worker qualification scores
  worker_repeated_tests <- 0
  total_repeated_tests <- 0
  worker_id_list <- unique(df_consent$worker_id)
  for (id in worker_id_list) {
    qualification_score_list <- df_consent[df_consent$worker_id == id,"qualification_score"]
    number_tests <- length(qualification_score_list)
    if(number_tests>1){
     # print(number_tests)
      worker_repeated_tests <- worker_repeated_tests+1
      total_repeated_tests <- total_repeated_tests +number_tests
    }
    average_score <- ave(qualification_score_list)
    df_consent[df_consent$worker_id==id,"qualification_score"] <- average_score
  }
  
 # print(paste("worker_repeated_tests:",worker_repeated_tests))
 # print(paste("total_repeated_tests:",total_repeated_tests))
 # There were no repeated tests!
  
  #test PASSED
  for (id in worker_id_list) {
    qualification_score_list <- df_consent[df_consent$worker_id == id,"qualification_score"]
    different_scores <- unique(qualification_score_list)
    if(length(different_scores)>1)
      print(id)
  }
  
  
  #-----------------------
  #--------------------------
  "ITEM RESPONSE MODEL SCORES
  Merge Score factors computed through IRT Model fitting"
  df_irt <- read.csv(paste0(path,"//irt//","E1_QualificationTest_IRT.csv"))
  df_irt <-  dplyr::select(df_irt, worker_id,irt_qualification_score) #need file_name, because a few workers have more than one score.
  df_irt$worker_id <- as.factor(df_irt$worker_id) #convert to factor, so I can join with worker_id column
  df_consent$worker_id <- as.factor(df_consent$worker_id) #convert to factor, so I can join with worker_id column
  df_consent <- left_join(x=df_consent,y=df_irt,keep=FALSE, by=c("worker_id"="worker_id"))
  dim(df_consent) 
  df_consent <- rename(df_consent,adjusted_score=irt_qualification_score)
  
  # Averaging worker adjusted_scores
  worker_id_list <- unique(df_consent$worker_id)
  for (id in worker_id_list) {
    adjusted_score_list <- df_consent[df_consent$worker_id == id,"adjusted_score"]
    average_score <- ave(adjusted_score_list)
    df_consent[df_consent$worker_id==id,"adjusted_score"] <- average_score
  }
  
  #test PASSED
  worker_id_list <- unique(df_consent$worker_id)
  for (id in worker_id_list) {
    qualification_score_list <- df_consent[df_consent$worker_id == id,"adjusted_score"]
    different_scores <- unique(qualification_score_list)
    if(length(different_scores)>1)
      print(paste0(id,"=",different_scores))
  }
  
  #-----------------------
  "GENDER"
  
  df_consent$gender<- factor(df_consent$gender, 
                             levels = c("Female","Male","Prefer_not_to_tell")
  )
  
  df_consent$gender_id<- factor(df_consent$gender, 
                                levels=levels(df_consent$gender),
                                labels = c(1:3)
  )
  #-----------------------
  "COUNTRY"
  
  df_consent$country <- unlist(lapply(df_consent$country, function(v) {
    if (is.character(v)) return(toupper(v))
    else return(v)
  }))
  
  df_consent$country <- gsub("UNITED STATES OF AMERICA","US",df_consent$country)
  df_consent$country <- gsub("UNITED STATES","US",df_consent$country)
  df_consent$country <- gsub("UNITES STATES","US",df_consent$country)
  df_consent$country <- gsub("AMERICA","US",df_consent$country)
  df_consent$country <- gsub("LOS ANGELES","US",df_consent$country)
  df_consent$country <- gsub("DALLAS","US",df_consent$country)
  df_consent$country <- gsub("WASHINGTON","US",df_consent$country)
  df_consent$country <- gsub("ILLINOIS","US",df_consent$country)
  df_consent$country <- gsub("U\\.S\\.A\\.","US",df_consent$country)
  df_consent$country <- gsub("U\\.S\\.","US",df_consent$country)
  df_consent$country <- gsub("THE US","US",df_consent$country)
  df_consent$country <- gsub("USA","US",df_consent$country)
  df_consent$country <- gsub("SD","S",df_consent$country)
  df_consent$country <- gsub("33","OTHER",df_consent$country)
  
  
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
  
  #---------------------
  # STUDENT, NON-STUDENT, OTHER labels
  
  if(load_is_student){
    df_aux <- read.csv(paste0(path,"is_student_E1.csv"))
    df_aux <- df_aux  %>% select("worker_id","is_student")
    #Create labels as three types of professions (student, non-student, other)
    df_aux[df_aux$is_student=="0" & !is.na(df_aux$is_student),"profession"] <- "non-student"
    df_aux[df_aux$is_student=="1" & !is.na(df_aux$is_student),"profession"] <- "student"
    df_aux[is.na(df_aux$is_student),]$profession <- "other"
    df_aux$profession <- factor(df_aux$profession)
    
    df_consent <- dplyr::left_join(df_consent,df_aux,
                                   by=c("worker_id"="worker_id"),
                                   keep=FALSE,copy=FALSE)
  }
  
  
  #------------------------------------------------------------
  #DURATION of TEST
  #Convert to minutes
  df_consent$test_duration <- df_consent$test_duration/(1000*60)
  
  "TEST DURATION OUTLIERS
  We study the distribution of test duration by comparing their quartiles and the wiskers.
  The latter have values > 3rd quartile + 1.5*interquartile range. 
  The interquartile range is the difference between the 2nd and 3rd quartiles
  "
  
  profession_list <- as.character(unique(df_consent$profession))
  
  computeMedians <- function(prof){
    median(df_consent[df_consent$profession==prof,]$test_duration)
  }
  medians_list <- lapply(profession_list, computeMedians)
  
  #Initializes a dataframe with profession names in first column and zeros for the remaining columns
  df_quantiles <- data.frame(matrix(data=c(profession_list,rep(0,12)),ncol=5,nrow = 3, byrow = FALSE),
                             stringsAsFactors=FALSE) #initialize with all zeros
  colnames(df_quantiles) <- c("profession","median","q2","q3","upper_wisker")
  
  #Quantiles
  computeQuantiles <- function(prof){
    quantile(df_consent[df_consent$profession==prof,]$test_duration)
  }
  quantile_list <- lapply(profession_list,computeQuantiles)
  
  for(i in c(1:length(profession_list))){
    values <- unlist(quantile_list[i])
    prof <- profession_list[i]
    df_quantiles[df_quantiles$profession==prof,]$q2 <- as.numeric(values[[2]])
    df_quantiles[df_quantiles$profession==prof,]$median <- as.numeric(values[[3]])
    df_quantiles[df_quantiles$profession==prof,]$q3 <- as.numeric(values[[4]])
    inter_quartile <- as.numeric(values[[4]] - values[[2]])
    df_quantiles[df_quantiles$profession==prof,]$upper_wisker <- as.numeric(values[[4]] + 1.5 * inter_quartile)
  } 
  
  #Generate BOXPLOTS
  #Do Statistical tests to check if distributions are statistically distinct (before and after outlier processing)
  #Statistical tests: null-hypothesis tests of equal means and also Kolmogorov-Smirnov test.
  #    profession  median                q2               q3     upper_wisker
  # 1       other 1.66485 0.800966666666667 3.04553333333333 6.41238333333333
  # 2 non-student 2.11485             0.764 3.95444166666667 8.74010416666667
  # 3     student  1.9874           0.66665 3.56496666666667 7.91244166666667
  
  #Quantiles
  values <- quantile(df_consent$test_duration)
  q2 <- values[[2]]
  median <- values[[3]]
  q3 <- values[[4]]
  inter_quartile <- values[[4]] - values[[2]]
  upper_wisker <- values[[4]] + 1.5 * inter_quartile
  lower_wisker <- values[[2]] -1.5 *inter_quartile #NOT USED
  
 "A reasonable time for the qualification test in E1 is 20 min, which gives 5 min per question,
  which is the average time people took to answer the code inspection tasks. We decided to 
  use twice this average as a threshold, hence 40min

  Instead of removing the datapoints we replaced the values that were above 40 min to 
  the median of each professional group, so we mitigated the impact on the distribution of test duration.
  "
  df_consent[df_consent$test_duration>40,]$test_duration <- as.numeric(median)
  
 
  #---------------------------------------
  "FAST TEST ANSWER MEMBERSHIP
  Merge the membership column that tells whether a worker is part of the fast or slow test takers.
  This column was produced by building a Gaussian Mixture model.
  "
  
  df_fastMembership <- read.csv(paste0(path,"mixture_model//","E1_consent_with_testDuration_fastMembership.csv"))
  
  df_fastMembership <- 
    dplyr::select(df_fastMembership,
                  worker_id,
                  profession,
                  testDuration_fastMembership,
                  is_fast
    );
  df_consent <- left_join(df_consent,df_fastMembership,by=c("worker_id","profession"),
                          copy= FALSE)
  
  source("C://Users//Christian//Documents//GitHub//EM_GaussianMixtureModel_TaskDurations//3.speed_classification//Label_FastSlowMembership_Test.R")
  #df_test <- compute_50percent_label(df_consent)
  #table(dplyr::select(df_test,is_fast, profession))
  #         profession
  # is_fast non-student other student
  # FALSE         252  1860      66
  # TRUE          154  1353      11
  
  #df_test <- compute_media_label(df_consent)
  #table(dplyr::select(df_test,is_fast, profession))
  #  MEDIAN       profession
  # is_fast non-student other student
  # FALSE         203  1606      38
  # TRUE          203  1607      39

  df_consent <- compute_mean_label(df_consent)
  #table(dplyr::select(df_consent,is_fast, profession))
  # MEAN          profession
  # is_fast non-student other student
  # FALSE         244  1841      51
  # TRUE          162  1372      26
  
  #MEAN as threshold seems more more balanced than the fixed 0.5 threshold
  #------------------------------------------------------------------------
  
  print(paste0("Loaded ",dim(df_consent)[1], " rows."," Results are in df_consent"))
  
  #---------------------
  #END
  #---------------------
  
  return(df_consent)
  
}

