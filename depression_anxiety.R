
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

#################################################################################
# Depression and anxiety
#################################################################################

# Level of depression.
# 0-4 = no depression
# 5-9 = minimal depression
# 10-14 = mild depression
# 15-19 = major depression
# > 20 = severe major depression
mean(dataset$Depression_total, na.rm=TRUE) 
response_rate(dataset$Depression_total)
boxplot(dataset$Depression_total)
std(dataset$Depression_total)

# Level of impairment from depression (scale: 0-6)
mean(dataset$Depression_interference, na.rm=TRUE) 
response_rate(dataset$Depression_interference)
boxplot(dataset$Depression_interference)
std(dataset$Depression_interference)
hist(na.omit(dataset$Depression_interference), breaks = c(-1,0,1,2,3,4,5,6), 
     xlab="Difficulty Working and Socializing due to Depression",
     freq = FALSE, labels = c("Not Difficult","","","","","", "Very Difficult"), 
     main="Depression Interference")

# Level of anxiety 
# 5-9 = mild anxiety; 
# 10-14 = moderate anxiety; 
# 15-21 = severe anxiety
mean(dataset$Anxiety_total, na.rm=TRUE) 
response_rate(dataset$Anxiety_total)
boxplot(dataset$Anxiety_total)
std(dataset$Anxiety_total)

# Level of impairment from anxiety (scale: 0-6)
mean(dataset$Anxiety_interference, na.rm=TRUE) 
response_rate(dataset$Anxiety_interference)
boxplot(dataset$Anxiety_interference)
std(dataset$Anxiety_interference)
hist(na.omit(dataset$Anxiety_interference), breaks = c(-1,0,1,2,3,4,5,6), 
     xlab="Difficulty Working and Socializing due to Anxiety",
     freq = FALSE, labels = c("Not Difficult","","","","","", "Very Difficult"), 
     main="Anxiety Interference")





