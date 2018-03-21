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
# Mental Health Stigma
#################################################################################

# General MH stigma (scale: 3-18)
mean(dataset$Stigma_total, na.rm=TRUE) 
var(dataset$Stigma_total, na.rm = TRUE)
std(dataset$Stigma_total)
response_rate(dataset$Stigma_total)
boxplot(dataset$Stigma_total)

# The extent to which an individual conceals their mental illness (scale: 3-18)
mean(dataset$Concealment_total, na.rm=TRUE) 
var(dataset$Concealment_total, na.rm = TRUE)
std(dataset$Concealment_total)
response_rate(dataset$Concealment_total)
boxplot(dataset$Concealment_total)
