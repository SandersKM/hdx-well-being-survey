
library(readxl) #library used to import Excel data
# import the Hendrix Well Being Survey Data (change location for your own HWBS Data)
dataset <- read_excel("C:/Users/kates/Desktop/HWBS_STUDENTS_2017_condensed.xlsx")
rownum <- 531 #total number of responses recorded

#function for determining the response rate (% data that was not NA)
response_rate <- function(col){
  rr <- (rownum - sum(is.na(col)))/ rownum 
  return(rr)
}

#################################################################################
# Non-suicidal self-injury and eating behavior
#################################################################################

# Frequency of non-suicidal self-injury as percentage of sample
# who engaged in at least one type of NSSI over the past year 
NSSI_total_tbl <- table(na.omit(dataset$NSSI_total))
sum(NSSI_total_tbl[names(NSSI_total_tbl)>0])/sum(NSSI_total_tbl)

# Level of eating behavior impairment 
# (scale:0-45) >16 = At Risk
mean(dataset$ED_total, na.rm=TRUE) 
response_rate(dataset$ED_total)

