library(data.table)
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
# MH and Academic Performance
#################################################################################

#In the past month, how many days have you felt that emotional or mental health 
#difficulties have hurt your academic performance?
#None; 0 days - 1
#1-2 days - 2
#3-5 days - 3
#6 or more days - 4
MH_academic_impact_tbl <- table(na.omit(dataset$MH_academic_impact)) 
MH_academic_impact_hist <- hist(dataset$MH_academic_impact,  breaks = c(0,1,2,3,4), 
                                freq = FALSE, labels = c("0", "1-2", "3-5", ">5"), 
                                xlab = "Days of Hurt Academic Performance", 
                                main = "MH Affecting Academic Performance in Past Month")

PH_academic_impact_tbl <- table(dataset$PH_academic_impact)
Eating_academic_impact_tbl <- table(dataset$Eating_academic_impact)
Substance_academic_impact_tbl <- (dataset$Substance_academic_impact)
Anxiety_academic_impact_tbl <- table(dataset$Anxiety_academic_impact)
Depression_academic_impact_tbl <- table(dataset$Depression_academic_impact)
Extracurricular_academic_impact_tbl <- table(dataset$Extracurricular_academic_impact)
HelpingOthers_academic_impact_tbl <- table(dataset$HelpingOthers_academic_impact)
Relationships_academic_impact_tbl <- table(dataset$Relationships_academic_impact)
Discrimination_academic_impact_tbl <- table(dataset$Discrimination_academic_impact)

