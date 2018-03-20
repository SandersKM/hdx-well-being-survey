#Libraries for graphing in R
library(rlang)
library(ggplot2)

library(readxl) #library used to import Excel data
# import the Hendrix Well Being Survey Data (change location for your own HWBS Data)
dataset <- read_excel("C:/Users/kates/Desktop/HWBS_STUDENTS_2017_condensed.xlsx")
rownum <- 531 #total number of responses recorded. Change with new data!

# Function for getting frequency of how many people checked each box
# Erase "rownum" from return if you just want counts and not freqency.
freq_bool <- function(col){
  tbl <- table(na.omit(col))
  return(sum(tbl[names(tbl)==1])/sum(tbl))
}

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



# Count how many ppl checked each kind of stressor
Stressor_schoolfinances_freq <- freq_bool(dataset$Stressor_schoolfinances)
Stressor_schoolwork_freq <-freq_bool(dataset$Stressor_schoolwork)
Stressor_employment_freq <-freq_bool(dataset$Stressor_employment)
Stressor_romrelationships_freq <-freq_bool(dataset$Stressor_romrelationships)
Stressor_friendrelationships_freq <-freq_bool(dataset$Stressor_friendrelationships)
Stressor_familyrelationships_freq <-freq_bool(dataset$Stressor_familyrelationships)
Stressor_roommaterelationships_freq <-freq_bool(dataset$Stressor_roommaterelationships)
Stressor_illnessinjury_freq <-freq_bool(dataset$Stressor_illnessinjury)
Stressor_changelivingcond_freq <-freq_bool(dataset$Stressor_changelivingcond)
Stressor_professors_freq <-freq_bool(dataset$Stressor_professors)
Stressor_familypressure_freq <-freq_bool(dataset$Stressor_familypressure)
Stressor_familyresponsibilities_freq <-freq_bool(dataset$Stressor_familyresponsibilities)
Stressor_academicperf_freq <-freq_bool(dataset$Stressor_academicperf)
Stressor_academicplanning_freq <-freq_bool(dataset$Stressor_academicplanning)
Stressor_extracurriculars_freq <-freq_bool(dataset$Stressor_extracurriculars)
Stressor_timemange_freq <-freq_bool(dataset$Stressor_timemange)
Stressor_timeforself_freq <-freq_bool(dataset$Stressor_timeforself)
Stressor_future_freq <-freq_bool(dataset$Stressor_future)
Stressor_sleep_freq <-freq_bool(dataset$Stressor_sleep)
Stressor_appearance_freq <-freq_bool(dataset$Stressor_appearance)
Stressor_PH_freq <-freq_bool(dataset$Stressor_PH)
Stressor_MH_freq <-freq_bool(dataset$Stressor_MH)
Stressor_lonely_freq <-freq_bool(dataset$Stressor_lonely)
Stressor_social_freq <-freq_bool(dataset$Stressor_social)
Stressor_safety_freq <-freq_bool(dataset$Stressor_safety)
Stressor_SNS_freq <-freq_bool(dataset$Stressor_SNS)
Stressor_identity_freq <-freq_bool(dataset$Stressor_identity)
Stressor_politics_freq <-freq_bool(dataset$Stressor_politics)
Stressor_discrim_freq <-freq_bool(dataset$Stressor_discrim)
Stressor_supportingothersMH_freq <-freq_bool(dataset$Stressor_supportingothersMH)
Stressor_other_freq <-freq_bool(dataset$Stressor_other)

# put stressors in dataframe
Stressors_all <- data.frame(Stressor_academicperf_freq, Stressor_academicplanning_freq, 
                            Stressor_appearance_freq, Stressor_changelivingcond_freq, 
                            Stressor_discrim_freq, Stressor_employment_freq, 
                            Stressor_extracurriculars_freq, Stressor_familypressure_freq, 
                            Stressor_familyrelationships_freq, Stressor_future_freq, 
                            Stressor_familyresponsibilities_freq,Stressor_romrelationships_freq, 
                            Stressor_friendrelationships_freq, Stressor_identity_freq, 
                            Stressor_illnessinjury_freq, Stressor_lonely_freq, 
                            Stressor_MH_freq, Stressor_other_freq, Stressor_PH_freq,
                            Stressor_politics_freq, Stressor_professors_freq, 
                            Stressor_roommaterelationships_freq, Stressor_safety_freq, 
                            Stressor_schoolfinances_freq, Stressor_schoolwork_freq,
                            Stressor_sleep_freq, Stressor_SNS_freq, Stressor_social_freq, 
                            Stressor_supportingothersMH_freq, Stressor_timeforself_freq, 
                            Stressor_timemange_freq)

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

#Looking at Correlations and Covariances 
# using Pearson's Method and casewise deletions for missing data

# Between Overall Stress and Overall Mental Health
cor(dataset$Overall_stress, dataset$Overall_MH, use="complete.obs", method="pearson")
cov(dataset$Overall_stress, dataset$Overall_MH, use="complete.obs", method="pearson")

# Between Overall Stress and Overall Physical Health
cor(dataset$Overall_stress, dataset$Overall_PH, use="complete.obs", method="pearson")
cov(dataset$Overall_stress, dataset$Overall_PH,  use="complete.obs", method="pearson")

# Between Overall Stress and Positive Emotions
cor(dataset$Overall_stress, dataset$PE_avg, use="complete.obs", method="pearson")
cov(dataset$Overall_stress, dataset$PE_avg, use="complete.obs", method="pearson")

#################################################################################
# Coping with Stress
#################################################################################

Coping_relspirit_freq <- freq_bool(dataset$Coping_relspirit)
Coping_Tvmovies_freq <- freq_bool(dataset$Coping_Tvmovies)
Coping_internet_freq <- freq_bool(dataset$Coping_internet)
Coping_read_freq <- freq_bool(dataset$Coping_read)
Coping_friendsfamily_freq <- freq_bool(dataset$Coping_friendsfamily)
Coping_outdoors_freq <- freq_bool(dataset$Coping_outdoors)
Coping_sleep_freq <- freq_bool(dataset$Coping_sleep)
Coping_hobby_freq <- freq_bool(dataset$Coping_hobby)
Coping_caffeine_freq <- freq_bool(dataset$Coping_caffeine)
Coping_cigarettes_freq <- freq_bool(dataset$Coping_cigarettes)
Coping_marijuana_freq <- freq_bool(dataset$Coping_marijuana)
Coping_illegaldrugs_freq <- freq_bool(dataset$Coping_illegaldrugs)
Coping_food_freq <- freq_bool(dataset$Coping_food)
Coping_videogames_freq <- freq_bool(dataset$Coping_videogames)
Coping_sports_freq <- freq_bool(dataset$Coping_sports)
Coping_meditateyoga_freq <- freq_bool(dataset$Coping_meditateyoga)
Coping_Mhprofessional_freq <- freq_bool(dataset$Coping_Mhprofessional)
Coping_writing_freq <- freq_bool(dataset$Coping_writing)
Coping_planning_freq <- freq_bool(dataset$Coping_planning)
Coping_talking_freq <- freq_bool(dataset$Coping_talking)
Coping_other_freq <- freq_bool(dataset$Coping_other)
Coping_exercise_freq <- freq_bool(dataset$Coping_exercise)
Coping_music_freq <- freq_bool(dataset$Coping_music)
Coping_alcohol_freq <- freq_bool(dataset$Coping_alcohol)
Coping_medication_freq <- freq_bool(dataset$Coping_medication)

Coping_all <- data.frame(Coping_Tvmovies_freq,Coping_internet_freq,Coping_read_freq,
                         Coping_friendsfamily_freq,Coping_outdoors_freq, Coping_sleep_freq,
                         Coping_hobby_freq,Coping_caffeine_freq,Coping_cigarettes_freq,
                         Coping_marijuana_freq, Coping_illegaldrugs_freq,
                         Coping_food_freq,Coping_videogames_freq,Coping_sports_freq,
                         Coping_meditateyoga_freq,Coping_Mhprofessional_freq,
                         Coping_writing_freq,Coping_planning_freq,Coping_talking_freq,
                         Coping_other_freq,Coping_exercise_freq,Coping_music_freq,
                         Coping_alcohol_freq,Coping_medication_freq,Coping_relspirit_freq)

Coping_all_sorted <- sort(Coping_all, decreasing = TRUE)
Coping_top_5 <- Coping_all_sorted[order(Coping_all_sorted, decreasing = TRUE)[1:5]]

#percentage of people who meditate:
Coping_meditateyoga_freq

