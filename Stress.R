#Libraries for graphing in R
library(rlang)
library(ggplot2)

library(readxl) #library used to import Excel data
# import the Hendrix Well Being Survey Data (change location for your own HWBS Data)
dataset <- read_excel("C:/Users/kates/Desktop/HWBS_STUDENTS_2017_condensed.xlsx")
rownum <- 531 #total number of responses recorded. Change with new data!

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

# Function for getting frequency of how many people checked each stressor
# Erase "rownum" from return if you just want counts and not freqency.
count_stressors <- function(category){
  count <- 0
  for(i in 1:rownum){
    if (category[i] == 1){
      count <- count + 1
    }
  }
  return(count/rownum)
}

# Count how many ppl checked each kind of stressor
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

# put stressors in dataframe
Stressors_all <- data.frame(Stressor_academicperf_count, Stressor_academicplanning_count, 
                            Stressor_appearance_count, Stressor_changelivingcond_count, 
                            Stressor_discrim_count, Stressor_employment_count, 
                            Stressor_extracurriculars_count, Stressor_familypressure_count, 
                            Stressor_familyrelationships_count, Stressor_future_count, 
                            Stressor_familyresponsibilities_count,Stressor_romrelationships_count, 
                            Stressor_friendrelationships_count, Stressor_identity_count, 
                            Stressor_illnessinjury_count, Stressor_lonely_count, 
                            Stressor_MH_count, Stressor_other_count, Stressor_PH_count,
                            Stressor_politics_count, Stressor_professors_count, 
                            Stressor_roommaterelationships_count, Stressor_safety_count, 
                            Stressor_schoolfinances_count, Stressor_schoolwork_count,
                            Stressor_sleep_count, Stressor_SNS_count, Stressor_social_count, 
                            Stressor_supportingothersMH_count, Stressor_timeforself_count, 
                            Stressor_timemange_count)

# sort columns in order of highest stressors to lowest stressor.
Stressors_all_sorted <- sort(Stressors_all, decreasing = TRUE)

# Top 5 Stressors
Stressors_top_5 <- Stressors_all_sorted[order(Stressors_all_sorted, decreasing = TRUE)[1:5]]

#bad plot of all stressors (needs improvement)
stressor_values_sorted_vertex <- as.numeric(Stressors_all_sorted[1,])
stressor_names_sorted_vertex <- c(colnames(Stressors_all_sorted))
stressor_barplot_sorted <-  barplot(stressor_values_sorted_vertex,
                                names.arg = stressor_names_sorted_vertex,  
                                xlab = "Stressor",ylab = "Frequency",main = "Stressors",las=2)
stressor_values_vertex_top_5 <- as.numeric(Stressors_top_5[1,])
stressor_names_vertex_top_5 <- c(colnames(Stressors_top_5))
stressor_barplot_top_5 <-  barplot(stressor_values_vertex_top_5,
                                    names.arg = stressor_names_vertex_top_5 ,  
                                    xlab = "Stressor",ylab = "Frequency",main = "Stressors",las=2)

#################################################################################
# Peceptions of Stress
#################################################################################


# Beliefs about how stress affects people's health
# -4 = Bad, 4 = Good
mean(dataset$Stress_and_health, na.rm=TRUE) 
Stress_and_health_var <- var(dataset$Stress_and_health, na.rm = TRUE)
sqrt(Stress_and_health_var)
response_rate(dataset$Stress_and_health)
boxplot(dataset$Stress_and_health)

# To what extent do you think stress negatively affects your personal happiness
# Scale (0-6)
mean(dataset$Stress_and_happiness, na.rm=TRUE) 
Stress_and_happiness_var <- var(dataset$Stress_and_happiness, na.rm = TRUE)
sqrt(Stress_and_happiness_var)
response_rate(dataset$Stress_and_happiness)
boxplot(dataset$Stress_and_happiness)

# To what extent do you think stress negatively affects your personal Mental Health
# Scale (0-6)
mean(dataset$Stress_and_MH, na.rm=TRUE) 
Stress_and_MH_var <- var(dataset$Stress_and_MH, na.rm = TRUE)
sqrt(Stress_and_MH_var)
response_rate(dataset$Stress_and_MH)
boxplot(dataset$Stress_and_MH)

# To what extent do you think stress negatively affects your personal Physical Health
# Scale (0-6)
mean(dataset$Stress_and_PH, na.rm=TRUE) 
Stress_and_PH_var <- var(dataset$Stress_and_PH, na.rm = TRUE)
sqrt(Stress_and_PH_var)
response_rate(dataset$Stress_and_PH)
boxplot(dataset$Stress_and_PH)

#################################################################################
# Coping with Stress
#################################################################################