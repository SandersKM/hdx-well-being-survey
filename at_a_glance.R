
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
for(n in 1:length(dataset$Overall_MH_R)){
  if(!is.na(dataset$Overall_MH_R[n]) && dataset$Overall_MH_R[n] > 5){
    dataset$Overall_MH_R[n] <- NA} #This gets rid of the 6s in the data...
}
mean(dataset$Overall_MH_R, na.rm=TRUE) 
response_rate(dataset$Overall_MH_R)
Overall_MH_R_var <- var(dataset$Overall_MH_R, na.rm = TRUE)
sqrt(Overall_MH_R_var) #Standard Deviation
hist(na.omit(dataset$Overall_MH_R), breaks = c(0,1,2,3,4,5), xlab="Description of Mental Health",
     freq = FALSE, labels = c("Very Poor", "Poor", "Fair", "Good", "Excellent"), 
     main="Overall Mental Health")

#“I see myself as a person with mental illness” (scale:1-6)
mean(dataset$MI_identity_R, na.rm=TRUE) 
response_rate(dataset$MI_identity_R)
MI_identity_R_var <- var(dataset$MI_identity_R, na.rm = TRUE)
sqrt(MI_identity_R_var)#Standard Deviation
hist(na.omit(dataset$Overall_MH_R), breaks = c(0,1,2,3,4,5,6), 
     xlab="I see myself as a person with mental illness.",
     freq = FALSE, labels = c("Strongly Disagree", "Disagree", "Somewhat Disagree",
     "Somewhat Agree", "Agree", "Strongly Agree"), 
     main="Mental Illness Identity")

#overall physical health (scale: 1-5)
for(n in 1:length(dataset$Overall_PH_R)){
  if(!is.na(dataset$Overall_PH_R[n]) && dataset$Overall_PH_R[n] > 5){
    dataset$Overall_PH_R[n] <- NA} #This gets rid of the 6s in the data...
}
mean(dataset$Overall_PH_R, na.rm=TRUE) 
response_rate(dataset$Overall_PH_R)
Overall_PH_R_var <- var(dataset$Overall_PH_R, na.rm = TRUE)
sqrt(Overall_PH_R_var)#Standard Deviation
hist(na.omit(dataset$Overall_PH_R), breaks = c(0,1,2,3,4,5), xlab="Description of Physical Health",
     freq = FALSE, labels = c("Very Poor", "Poor", "Fair", "Good", "Excellent"), 
     main="Overall Physical Health")

#overall stress (scale: 1-10)
mean(dataset$Overall_stress, na.rm=TRUE) 
response_rate(dataset$Overall_stress)
Overall_stress_var <- var(dataset$Overall_stress, na.rm = TRUE)
sqrt(Overall_stress_var)#Standard Deviation
boxplot(dataset$Overall_stress) #Having less low stress is an outlier.
median(dataset$Overall_stress, na.rm = TRUE)
hist(na.omit(dataset$Overall_stress), breaks = c(0,1,2,3,4,5,6,7,8,9,10), xlab="Average Stress Level in the Past Month",
     freq = FALSE, labels = c("No Stress","","","","","","","","","Extreme Stress"), 
     main="Overall Stress")

#hours per week exercising (open-ended)
min(dataset$Hours_exercising, na.rm=TRUE) #minimum
max(dataset$Hours_exercising, na.rm=TRUE) #maximum
mean(dataset$Hours_exercising, na.rm=TRUE) #including outliers
boxplot(dataset$Hours_exercising) #Boxplot with outliers
response_rate(dataset$Hours_exercising)
Q1_hours_exercising <- summary(dataset$Hours_exercising)[["1st Qu."]]
Q3_hours_exercising <-summary(dataset$Hours_exercising)[["3rd Qu."]]
IQR_hours_exercising <- Q3_hours_exercising - Q1_hours_exercising
Hours_exercising_var <- var(dataset$Hours_exercising, na.rm = TRUE)
sqrt(Hours_exercising_var) #Standard Deviation

#excluded values above 60 for hours of exercise per week
no_extreme_outliers <- outlierReplace(dataset, "Hours_exercising",
                                      which(dataset$Hours_exercising > 60), NA)
Hours_exercising_no_extreme_outliers <- no_extreme_outliers$Hours_exercising
min(Hours_exercising_no_extreme_outliers, na.rm=TRUE) #minimum
max(Hours_exercising_no_extreme_outliers, na.rm=TRUE) #maximum
mean(Hours_exercising_no_extreme_outliers, na.rm=TRUE) #including all but maxiumum outlier
Hours_exercising_no_extreme_outliers_var <- var(Hours_exercising_no_extreme_outliers, na.rm = TRUE)
sqrt(Hours_exercising_no_extreme_outliers_var)
boxplot(Hours_exercising_no_extreme_outliers) #Boxplot with outliers
response_rate(Hours_exercising_no_extreme_outliers)
Hours_exercising_no_extreme_outliers_q1 <- summary(Hours_exercising_no_extreme_outliers)[["1st Qu."]]
Hours_exercising_no_extreme_outliers_q3  <-summary(Hours_exercising_no_extreme_outliers)[["3rd Qu."]]
Hours_exercising_no_extreme_outliers_q3  - Hours_exercising_no_extreme_outliers_q1 #IQR


# typical hours of sleep per night (open-ended)
min(dataset$Hours_sleep, na.rm=TRUE) #minimum
max(dataset$Hours_sleep, na.rm=TRUE) #maximum
mean(dataset$Hours_sleep, na.rm=TRUE) 
response_rate(dataset$Hours_sleep)
Hours_sleep_var <- var(dataset$Hours_sleep, na.rm = TRUE)
sqrt(Hours_sleep_var)#Standard Deviation


#how respondents feel the campus environment at Hendrix 
#impacts students’ mental health (scale: -3 to +3)
mean(dataset$HDX_MH_impact, na.rm=TRUE) 
response_rate(dataset$HDX_MH_impact)
boxplot(dataset$HDX_MH_impact) #the responses here are very symetrical 
HDX_MH_impact_var <- var(dataset$HDX_MH_impact, na.rm = TRUE)
sqrt(HDX_MH_impact_var)#Standard Deviation
hist(na.omit(dataset$HDX_MH_impact), breaks = c(-4,-3, -2, -1, 0, 1, 2, 3), 
     xlab="Campus Environment Impact on Mental and Emotional Health",
     freq = FALSE, labels = c("Very Negatively","","","","","","Very Positively"), 
     main="Hendrix Impact on Student Mental Health")
