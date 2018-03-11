library(readxl) #library used to import Excel data
# import the Hendrix Well Being Survey Data (change location for your own HWBS Data)
dataset <- read_excel("C:/Users/kates/Desktop/HWBS_STUDENTS_2017_condensed.xlsx")
rownum <- 531 #total number of responses recorded

#################################################################################
# Stress
#################################################################################

# overall stress (scale: 1-10)
mean(dataset$Overall_stress, na.rm=TRUE) 
Overall_stress_var<-var(dataset$Overall_stress, na.rm = TRUE)
sqrt(Overall_stress_var)
response_rate(dataset$Overall_stress)
 
# Percieved Stress Scale
# 0-13: Low Stress
# 14-26: Moderate Stress
# 27-40: High Stress
mean(dataset$Stress_total, na.rm=TRUE) 
Stress_total_var <- var(dataset$Stress_total, na.rm = TRUE)
sqrt(Stress_total_var)
response_rate(dataset$Stress_total)

count_stressors <- function(category){
  count <- 0
  for(i in 1:rownum){
    if (category[i] == 1){
      count <- count + 1
    }
  }
  return(count)
}

Stressor_schoolfinances_count <- count_stressors(dataset$Stressor_schoolfinances)
Stressor_schoolwork_count <-count_stressors(dataset$Stressor_schoolwork)
Stressor_employment_count <-count_stressors(dataset$Stressor_employment)
Stressor_romrelationships_count <-count_stressors(dataset$Stressor_romrelationships)
Stressor_friendrelationships_count <-count_stressors(dataset$Stressor_friendrelationships)
Stressor_familyrelationships_count <-count_stressors(dataset$Stressor_familyrelationships)
Stressor_roommaterelationships_count <-count_stressors(dataset$Stressor_roommaterelationships)
Stressor_illnessinjury_count <-count_stressors(dataset$Stressor_illnessinjury)
Stressor_changelivingcond_count <-count_stressors(dataset$Stressor_changelivingcond)
Stressor_professors_count <-count_stressors(dataset$Stressor_professors)
Stressor_familypressure_count <-count_stressors(dataset$Stressor_familypressure)
Stressor_familyresponsibilities_count <-count_stressors(dataset$Stressor_familyresponsibilities)
Stressor_academicperf_count <-count_stressors(dataset$Stressor_academicperf)
Stressor_academicplanning_count <-count_stressors(dataset$Stressor_academicplanning)
Stressor_extracurriculars_count <-count_stressors(dataset$Stressor_extracurriculars)
Stressor_timemange_count <-count_stressors(dataset$Stressor_timemange)
Stressor_timeforself_count <-count_stressors(dataset$Stressor_timeforself)
Stressor_future_count <-count_stressors(dataset$Stressor_future)
Stressor_sleep_count <-count_stressors(dataset$Stressor_sleep)
Stressor_appearance_count <-count_stressors(dataset$Stressor_appearance)
Stressor_PH_count <-count_stressors(dataset$Stressor_PH)
Stressor_MH_count <-count_stressors(dataset$Stressor_MH)
Stressor_lonely_count <-count_stressors(dataset$Stressor_lonely)
Stressor_social_count <-count_stressors(dataset$Stressor_social)
Stressor_safety_count <-count_stressors(dataset$Stressor_safety)
Stressor_SNS_count <-count_stressors(dataset$Stressor_SNS)
Stressor_identity_count <-count_stressors(dataset$Stressor_identity)
Stressor_politics_count <-count_stressors(dataset$Stressor_politics)
Stressor_discrim_count <-count_stressors(dataset$Stressor_discrim)
Stressor_supportingothersMH_count <-count_stressors(dataset$Stressor_supportingothersMH)
Stressor_other_count <-count_stressors(dataset$Stressor_other)



#################################################################################
# Peceptions of Stress
#################################################################################


#################################################################################
# Coping with Stress
#################################################################################