library(readxl) #library used to import Excel data
# import the Hendrix Well Being Survey Data (change location for your own HWBS Data)
dataset <- read_excel("C:/Users/kates/Desktop/HWBS_STUDENTS_2017_condensed.xlsx")
rownum <- 531 #total number of responses recorded

#function for determining the response rate (% data that was not NA)
response_rate <- function(col){
  rr <- (rownum - sum(is.na(col)))/ rownum 
  return(rr)
}


#function for standard deviation
std <- function(col){
  var <- var(col, na.rm = TRUE)
  return(sqrt(var))
}

#percentage with scores above 0:
percent_above_0 <- function(col){
  tbl <- table(na.omit(col))
  return(sum(tbl[names(tbl)>0])/sum(tbl))
}


#################################################################################
# Substance Use
#################################################################################


# Alcohol Use 
#Never - 1
#Once - 2
#2-4 times total - 3
#2-3 times per week - 4
#4 or more times per week - 5
mean(dataset$Alcohol_use, na.rm=TRUE) 
var(dataset$Alcohol_use, na.rm = TRUE)
std(dataset$Alcohol_use)
response_rate(dataset$Alcohol_use)
boxplot(dataset$Alcohol_use)
#histogram of how often respondents drink
hist(dataset$Alcohol_use, breaks = c(0,1,2,3,4,5), xlab="Drinks in the Past Month",
     freq = FALSE, labels = c("Never", "Once", "2-4 Times\nTotal", "2-3 Times\nPer Week", 
                              "> 4 Times\nPer Week"), main="Alcohol Use",mar = c(5, 2, 2, 2), ylim = range(0,0.5))

mean(dataset$Cigarette_use, na.rm=TRUE) 
var(dataset$Cigarette_use, na.rm = TRUE)
std(dataset$Cigarette_use)
response_rate(dataset$Cigarette_use)
boxplot(dataset$Cigarette_use)
#histogram of how often respondents smoke
hist(dataset$Cigarette_use, breaks = c(0,1,2,3,4,5), xlab="Cigarettes Per Day in the Past Month",
     freq = FALSE, labels = c("0", "< 1", "1-5", "Half Pack", 
                              "> 1 Pack"), main="Cigarette Use", ylim=range(0,1))


#Drinking score 
mean(dataset$Drinking, na.rm=TRUE) 
var(dataset$Drinking, na.rm = TRUE)
std(dataset$Drinking)
response_rate(dataset$Drinking)
boxplot(dataset$Drinking)
percent_above_0(dataset$Drinking)#Percentage with at risk drinking habits

#Drug use score
mean(dataset$DrugUse, na.rm=TRUE) 
var(dataset$DrugUse, na.rm = TRUE)
std(dataset$DrugUse)
response_rate(dataset$DrugUse)
boxplot(dataset$DrugUse)
percent_above_0(dataset$DrugUse)#Percentage with at risk drug use

#Stimulant Use Score
mean(dataset$StimulantUse, na.rm=TRUE) 
var(dataset$StimulantUse, na.rm = TRUE)
std(dataset$StimulantUse)
response_rate(dataset$StimulantUse)
boxplot(dataset$StimulantUse)
percent_above_0(dataset$StimulantUse)#Percentage with at risk stimulant use

