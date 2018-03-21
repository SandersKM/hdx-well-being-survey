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
# Interrelationships
#################################################################################

# Between Anxiety and Depression
cor(dataset$Depression_total, dataset$Anxiety_total, use="complete.obs", method="pearson")
cov(dataset$Depression_total, dataset$Anxiety_total, use="complete.obs", method="pearson")

#Between Hours of Sleep and Depression
cor(dataset$Depression_total, dataset$Hours_sleep, use="complete.obs", method="pearson")
cov(dataset$Depression_total, dataset$Hours_sleep, use="complete.obs", method="pearson")

#Between Hours of Sleep and anxiety
cor(dataset$Anxiety_total, dataset$Hours_sleep, use="complete.obs", method="pearson")
cov(dataset$Anxiety_total, dataset$Hours_sleep, use="complete.obs", method="pearson")

#Between Hours of Sleep and stress
cor(dataset$Overall_stress, dataset$Hours_sleep, use="complete.obs", method="pearson")
cov(dataset$Overall_stress, dataset$Hours_sleep, use="complete.obs", method="pearson")

#Between Hours of Sleep and Mental Heath Continuum score
cor(dataset$MHCSF_total, dataset$Hours_sleep, use="complete.obs", method="pearson")
cov(dataset$MHCSF_total, dataset$Hours_sleep, use="complete.obs", method="pearson")

#Between Hours of Sleep and Positive Emotion
cor(dataset$PE_avg, dataset$Hours_sleep, use="complete.obs", method="pearson")
cov(dataset$PE_avg, dataset$Hours_sleep, use="complete.obs", method="pearson")

#Between Hours of Sleep and resilience 
cor(dataset$Resilience_avg, dataset$Hours_sleep, use="complete.obs", method="pearson")
cov(dataset$Resilience_avg, dataset$Hours_sleep, use="complete.obs", method="pearson")

#Between Overall stress and Depression
cor(dataset$Overall_stress, dataset$Depression_total, use="complete.obs", method="pearson")
cov(dataset$Overall_stress, dataset$Depression_total, use="complete.obs", method="pearson")

#Between Overall stress and Anxiety
cor(dataset$Overall_stress, dataset$Anxiety_total, use="complete.obs", method="pearson")
cov(dataset$Overall_stress, dataset$Anxiety_total, use="complete.obs", method="pearson")

#Between Overall stress and Mental Health Continuum Score
cor(dataset$Overall_stress, dataset$MHCSF_total, use="complete.obs", method="pearson")
cov(dataset$Overall_stress, dataset$MHCSF_total, use="complete.obs", method="pearson")

#Between Overall stress and Positive Emotion
cor(dataset$Overall_stress, dataset$PE_avg, use="complete.obs", method="pearson")
cov(dataset$Overall_stress, dataset$PE_avg, use="complete.obs", method="pearson")

#Between Overall stress and Resilience
cor(dataset$Overall_stress, dataset$Resilience_avg, use="complete.obs", method="pearson")
cov(dataset$Overall_stress, dataset$Resilience_avg, use="complete.obs", method="pearson")

#Between Screen Time and Depression
cor(dataset$Hours_screentime, dataset$Depression_total, use="complete.obs", method="pearson")
cov(dataset$Hours_screentime, dataset$Depression_total, use="complete.obs", method="pearson")

#Between Screen Time and Anxiety
cor(dataset$Hours_screentime, dataset$Anxiety_total, use="complete.obs", method="pearson")
cov(dataset$Hours_screentime, dataset$Anxiety_total, use="complete.obs", method="pearson")

#Between Screen Time and Mental Health Continuum Score
cor(dataset$Hours_screentime, dataset$MHCSF_total, use="complete.obs", method="pearson")
cov(dataset$Hours_screentime, dataset$MHCSF_total, use="complete.obs", method="pearson")

#Between Screen Time and Positive Emotion
cor(dataset$Hours_screentime, dataset$PE_avg, use="complete.obs", method="pearson")
cov(dataset$Hours_screentime, dataset$PE_avg, use="complete.obs", method="pearson")

#Between Screen Time and Resilience
cor(dataset$Hours_screentime, dataset$Resilience_avg, use="complete.obs", method="pearson")
cov(dataset$Hours_screentime, dataset$Resilience_avg, use="complete.obs", method="pearson")

#Between Screen Time and Overall Stress
cor(dataset$Hours_screentime, dataset$Overall_stress, use="complete.obs", method="pearson")
cov(dataset$Hours_screentime, dataset$Overall_stress, use="complete.obs", method="pearson")
