library(readxl) #library used to import Excel data
# import the Hendrix Well Being Survey Data (change location for your own HWBS Data)
dataset <- read_excel("C:/Users/kates/Desktop/HWBS_STUDENTS_2017_condensed.xlsx")
rownum <- 531 #total number of responses recorded

#function for determining the response rate (% data that was not NA)
response_rate <- function(col){
  rr <- (rownum - sum(is.na(col)))/ rownum 
  return(rr)
}

#function for getting the standard deviation
std <- function(col){
  var <- var(col, na.rm = TRUE)
  return(sqrt(var))
}

#frequeny of people who answered "yes"
#percentage with scores above 0:
freq_bool <- function(col){
  tbl <- table(na.omit(col))
  return(sum(tbl[names(tbl)==1])/sum(tbl))
}

#################################################################################
# General Mental Health Knowledge
#################################################################################

#Recognizing someone in distress (1 = Strongly Agree - 6 = Strongly Disagree)
mean(dataset$Recognize_distress, na.rm=TRUE) 
var(dataset$Recognize_distress, na.rm = TRUE)
std(dataset$Recognize_distress)
response_rate(dataset$Recognize_distress)
boxplot(dataset$Recognize_distress)

#I feel confident in helping someone with a mental health problem.
#(1 = Strongly Agree - 6 = Strongly Disagree)
mean(dataset$Helping_confidence, na.rm=TRUE) 
var(dataset$Helping_confidence, na.rm = TRUE)
std(dataset$Helping_confidence)
response_rate(dataset$Helping_confidence)
boxplot(dataset$Helping_confidence)

#Knowing where to go for mental health help
#(1 = Strongly Agree - 6 = Strongly Disagree)
mean(dataset$MH_know_where_to_go, na.rm=TRUE) 
var(dataset$MH_know_where_to_go, na.rm = TRUE)
std(dataset$MH_know_where_to_go)
response_rate(dataset$MH_know_where_to_go)
boxplot(dataset$MH_know_where_to_go)

##Knowing where to go for physical health help
#(1 = Strongly Agree - 6 = Strongly Disagree)
mean(dataset$PH_know_where_to_go, na.rm=TRUE) 
var(dataset$PH_know_where_to_go, na.rm = TRUE)
std(dataset$PH_know_where_to_go)
response_rate(dataset$PH_know_where_to_go)
boxplot(dataset$PH_know_where_to_go)

#Percentage of students who have participated in MH training
freq_bool(na.omit(dataset$MH_training))
