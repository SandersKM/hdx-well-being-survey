
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
# At a glance
#################################################################################

#overall mental health  (scale:1-5)
mean(dataset$Overall_MH_R, na.rm=TRUE) 
response_rate(dataset$Overall_MH_R)

#“I see myself as a person with mental illness” (scale:1-6)
mean(dataset$MI_identity_R, na.rm=TRUE) 
response_rate(dataset$MI_identity_R)

#overall physical health (scale: 1-5)
mean(dataset$Overall_PH_R, na.rm=TRUE) 
response_rate(dataset$Overall_PH_R)

#overall stress (scale: 1-10)
mean(dataset$Overall_stress, na.rm=TRUE) 
response_rate(dataset$Overall_stress)

#hours per week exersising (open-ended)
min(dataset$Hours_exercising, na.rm=TRUE) #minimum
max(dataset$Hours_exercising, na.rm=TRUE) #maximum
mean(dataset$Hours_exercising, na.rm=TRUE) #including outliers
boxplot(dataset$Hours_exercising) #Boxplot with outliers
response_rate(dataset$Hours_exercising)
Q1_hours_exercising <- summary(dataset$Hours_exercising)[["1st Qu."]]
Q3_hours_exercising <-summary(dataset$Hours_exercising)[["3rd Qu."]]
IQR_hours_exercising <- Q3_hours_exercising - Q1_hours_exercising

# typical hours of sleep per night (open-ended)
min(dataset$Hours_sleep, na.rm=TRUE) #minimum
max(dataset$Hours_sleep, na.rm=TRUE) #maximum
mean(dataset$Hours_sleep, na.rm=TRUE) 
response_rate(dataset$Hours_sleep)

#how respondents feel the campus environment at Hendrix 
#impacts students’ mental health (scale: -3 to +3)
mean(dataset$HDX_MH_impact, na.rm=TRUE) 
response_rate(dataset$HDX_MH_impact)
boxplot(dataset$HDX_MH_impact) #the responses here are very symetrical 

#################################################################################
# 
#################################################################################

#################################################################################
# 
#################################################################################

#################################################################################
# 
#################################################################################
