"
Load data from E1 and create indexes for qualification_score, gender
"

library(farff)
library(ggplot2)
library(dplyr)

"--------------------------------------------------------------------
About the load function
is_student =1 adds a column with the classification of is_student_E1.csv
default = 0
"
load_consent_create_indexes <- function(load_is_student=0){
  
  #--------------------------
  "LOAD FILES"
  path <- "C://Users//Christian//Documents//GitHub//CausalModel_FaultUnderstanding//data//"
  dataset_E1 <- readARFF(paste0(path,"consent_consolidated_Experiment_1.arff"))
  df_consent <- data.frame(dataset_E1)
  dim(df_consent) #4776   16
  
  
  #------------------------
  "MISSING DATA"
  
  #Filter-out rows without test data
  dim(df_consent[is.na(df_consent$test1),]) #1077 are NA
  df_consent <- df_consent[complete.cases(df_consent[,"qualification_score"]),]
  dim(df_consent) #3699 are not NA.
  
  #-----------------------
  "IMPUTATION ERROR"
  
  #Filter-out 3 rows that have demographics data but did not pass the test (qualification score<2)
  #This should not have happened in normal execution, because demographics were collected only 
  #at the end of the experiment. 
  df_consent <- df_consent[!(df_consent$qualification_score<2 & !is.na(df_consent$years_programming)),]
  
  #-----------------------
  "QUALIFICATION_SCORE"
  
  "I cannot transform qualification score as a factor anymore for two reasons:
    - it is reasonable to assume a continuous scale 
    - there will be more than the integer values, because some workers will have averaged values that result
    from the fact that they took the more than one test.
  "
  
  # Averaging worker qualification scores
  worker_id_list <- unique(df_consent$worker_id)
  for (id in worker_id_list) {
    qualification_score_list <- df_consent[df_consent$worker_id == id,"qualification_score"]
    average_score <- ave(qualification_score_list)
    df_consent[df_consent$worker_id==id,"qualification_score"] <- average_score
  }
  
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
  
  #-----------------------
  #DURATION of TEST
  #Convert to minutes
  df_consent$test_duration <- df_consent$test_duration/(1000*60)
  
  
  "TEST DURATION OUTLIERS
Replace outliers in test_duration for median values.

A reasonable time for the qualification test in E1 is 20 min, which gives 5 min per question,
which is the average time people took to answer the code inspection tasks. 
Because the boxplot shows points that are above 20 min, which is more than 5 min per question

We consider as outliers all data points that are above wiskers in the boxplots. 
These datapoints have values > 3rd quartile + 1.5*interquartile range. 
The interquartile range is the difference between the 2nd and 3rd quartiles
"
  #Quantiles
  values <- quantile(df_consent$test_duration)
  q2 <- values[[2]]
  median <- values[[3]]
  q3 <- values[[4]]
  inter_quartile <- values[[4]] - values[[2]]
  upper_wisker <- values[[4]] + 1.5 * inter_quartile
  lower_wisker <- values[[2]] -1.5 *inter_quartile #NOT USED
  
  #Replace all values that are above 40 min to the median of each professional group
  upperwisker <- as.numeric(upper_wisker)
  median_value <- as.numeric(median)
  df_consent[df_consent$test_duration>40,]$test_duration <- median_value
  
  #---------------------
  #
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
  
  #---------------------
  
  print(paste0("Loaded ",dim(df_consent)[1], " rows."," Results are in df_consent"))
  
  #---------------------
  #END
  #---------------------
  
  return(df_consent)
  
}

