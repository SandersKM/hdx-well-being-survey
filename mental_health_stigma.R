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
Stigma_total_mean <- mean(dataset$Stigma_total, na.rm=TRUE) 
Stigma_total_var <- var(dataset$Stigma_total, na.rm = TRUE)
Stigma_total_std <- std(dataset$Stigma_total)
Stigma_total_rr <- response_rate(dataset$Stigma_total)
Stigma_total_box <- boxplot(dataset$Stigma_total)

# The extent to which an individual conceals their mental illness (scale: 3-18)
Concealment_total_mean <- mean(dataset$Concealment_total, na.rm=TRUE) 
Concealment_total_var <- var(dataset$Concealment_total, na.rm = TRUE)
Concealment_total_std <- std(dataset$Concealment_total)
Concealment_total_rr <- response_rate(dataset$Concealment_total)
Concealment_total_box <- boxplot(dataset$Concealment_total)
