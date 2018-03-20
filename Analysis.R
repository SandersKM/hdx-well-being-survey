# Main analysis file for the Hendrix Well Being Initiative Survey Analysis
# Created by Kate Sanders in Spring 2018
# After running this file, you can get the desired values by typing:
# Mean: categoryname_mean
# Variance: categoryname_var
# Standard Deviation: categoryname_std
# Response Rate: categoryname_rr
# Some of the categories other anayses accessed as: categoryname_anaysisname
# All of these values will appear automatically if you just run the subanalysis files

# You will need to instal these libraries is they aren't in your own RStudio. 
# Use: install.packages("packagename")

# Libraries for graphing in R
library(rlang)
library(ggplot2)
library(data.table) # for the set method
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
# At a glance
#################################################################################

#overall mental health  (scale:1-5)
Overall_MH_R_mean <- mean(dataset$Overall_MH_R, na.rm=TRUE)
Overall_MH_R_var <- var(dataset$Overall_MH_R, na.rm = TRUE)
Overall_MH_R_std <- std(dataset$Overall_MH_R)
Overall_MH_R_rr <- response_rate(dataset$Overall_MH_R)

#“I see myself as a person with mental illness” (scale:1-6)
MI_identity_R_mean <- mean(dataset$MI_identity_R, na.rm=TRUE) 
MI_identity_R_var <- var(dataset$MI_identity_R, na.rm = TRUE)
MI_identity_R_std <- std(dataset$MI_identity_R)
MI_identity_R_rr <- response_rate(dataset$MI_identity_R)

#overall physical health (scale: 1-5)
Overall_PH_R_mean <- mean(dataset$Overall_PH_R, na.rm=TRUE) 
Overall_PH_R_var <- var(dataset$Overall_PH_R, na.rm = TRUE)
Overall_PH_R_std <- sqrt(Overall_PH_R_var)
Overall_PH_R_rr <- response_rate(dataset$Overall_PH_R)

#overall stress (scale: 1-10)
Overall_stress_mean <- mean(dataset$Overall_stress, na.rm=TRUE) 
Overall_stress_var <- var(dataset$Overall_stress, na.rm = TRUE)
Overall_stress_std <- sqrt(Overall_stress_var)
Overall_stress_rr <- response_rate(dataset$Overall_stress)

outlierReplace = function(dataframe, cols, rows, newValue = NA) {
  if (any(rows)) {
    set(dataframe, rows, cols, newValue)
  }
}

#hours per week exercising (open-ended)
Hours_exercising_mean <- mean(dataset$Hours_exercising, na.rm=TRUE) #including outliers
Hours_exercising_var <- var(dataset$Hours_exercising, na.rm = TRUE)
Hours_exercising_std <- sqrt(Hours_exercising_var)
Hours_exercising_box <- boxplot(dataset$Hours_exercising) #Boxplot with outliers
Hours_exercising_rr <- response_rate(dataset$Hours_exercising)
Hours_exercising_summary <- summary(dataset$Hours_exercising)

#excluded values above 60 for hours of exercise per week
no_extreme_outliers <- outlierReplace(dataset, "Hours_exercising",
                                      which(dataset$Hours_exercising > 60), NA)
Hours_exercising_no_extreme_outliers <- no_extreme_outliers$Hours_exercising
Hours_exercising_no_extreme_outliers_mean <- mean(Hours_exercising_no_extreme_outliers, na.rm=TRUE) #including outliers
Hours_exercising_no_extreme_outliers_var <- var(Hours_exercising_no_extreme_outliers, na.rm = TRUE)
Hours_exercising_no_extreme_outliers_std <- sqrt(Hours_exercising_no_extreme_outliers_var)
Hours_exercising_no_extreme_outliers_box <- boxplot(Hours_exercising_no_extreme_outliers) #Boxplot with outliers
Hours_exercising_no_extreme_outliers_rr <- response_rate(Hours_exercising_no_extreme_outliers)
Hours_exercising_no_extreme_outliers_summary <- summary(Hours_exercising_no_extreme_outliers) #min, max, median, quartiles

# typical hours of sleep per night (open-ended)
Hours_sleep_summary <- summary(dataset$Hours_sleep)
Hours_sleep_mean <- mean(dataset$Hours_sleep, na.rm=TRUE) 
Hours_sleep_var <- var(dataset$Hours_sleep, na.rm = TRUE)
Hours_sleep_std <- sqrt(Hours_sleep_var)
Hours_sleep_rr <- response_rate(dataset$Hours_sleep)

#how respondents feel the campus environment at Hendrix 
#impacts students’ mental health (scale: -3 to +3)
HDX_MH_impact_mean <- mean(dataset$HDX_MH_impact, na.rm=TRUE) 
HDX_MH_impact_var <- var(dataset$HDX_MH_impact, na.rm = TRUE)
HDX_MH_impact_std <- sqrt(HDX_MH_impact_var)
HDX_MH_impact_rr <- response_rate(dataset$HDX_MH_impact)
HDX_MH_impact_box <- boxplot(dataset$HDX_MH_impact) #the responses here are very symetrical 

#################################################################################
# Positive mental health
#################################################################################

# Mental Health Continuum – Short Form (scale:0-70)
MHCSF_total_mean <- mean(dataset$MHCSF_total, na.rm=TRUE) 
MHCSF_total_var <- var(dataset$MHCSF_total,na.rm = TRUE)
MHCSF_total_std <- sqrt(MHCSF_total_var)
MHCSF_total_rr <- response_rate(dataset$MHCSF_total)
MHCSF_total_box <- boxplot(dataset$MHCSF_total)

#Level of positive emotions (scale 0-4)
PE_avg_mean <- mean(dataset$PE_avg, na.rm=TRUE) 
PE_avg_var <- var(dataset$PE_avg, na.rm = TRUE)
PE_avg_std <- sqrt(PE_avg_var)
PE_avg_rr <- response_rate(dataset$PE_avg)
PE_avg_box <- boxplot(dataset$PE_avg)

# Level of resilience (scale 1-5)
Resilience_avg_mean <- mean(dataset$Resilience_avg, na.rm=TRUE) 
Resilience_avg_var <- var(dataset$Resilience_avg, na.rm = TRUE)
Resilience_avg_std <- sqrt(Resilience_avg_var)
Resilience_avg_rr <- response_rate(dataset$Resilience_avg)
Resilience_avg_box <- boxplot(dataset$Resilience_avg)

# Level of satisfaction with personal 
#relationships at HDX (scale 1-5)
Relationship_satis_mean <- mean(dataset$Relationship_satis, na.rm=TRUE) 
Relationship_satis_var <- var(dataset$Relationship_satis, na.rm = TRUE)
Relationship_satis_std <- sqrt(Relationship_satis_var)
Relationship_satis_rr <- response_rate(dataset$Relationship_satis)
Relationship_satis_box <- boxplot(dataset$Relationship_satis)

# Level of belongingness (scale 9-54)
Belonging_total_mean <- mean(dataset$Belonging_total, na.rm=TRUE) 
Belonging_total_var <- var(dataset$Belonging_total, na.rm = TRUE)
Belonging_total_std <- sqrt(Belonging_total_var)
Belonging_total_rr <- response_rate(dataset$Belonging_total)
Belonging_total_box <- boxplot(dataset$Belonging_total)

######
#respondents who are considered “flourishing” 
######

#respondent must feel 1/3 well being symptoms 
#"every day" or "almost everyday" (4 or 5)
hedonic_wb <- function(i){
  if (!is.na(dataset$MHCSF1[i])){
    if(dataset$MHCSF1[i]==4 | dataset$MHCSF1[i]==5){        
      return(TRUE)
    }
  }
  if (!is.na(dataset$MHCSF2[i])){
    if(dataset$MHCSF2[i]==4 | dataset$MHCSF2[i]==5){       
        return(TRUE)
    }
  }
  if (!is.na(dataset$MHCSF3[i])){
    if(dataset$MHCSF3[i]==4 | dataset$MHCSF3[i]==5){
      return(TRUE)
    }
  }
  return(FALSE)
}

#respondent must feel 6/11 positive functioning symptoms 
#"every day" or "almost everyday" (4 or 5) 
positive_func <- function(i){
  sym_count <- 0
  if (!is.na(dataset$MHCSF4[i])){
    if(dataset$MHCSF4[i]==4 | dataset$MHCSF4[i]==5){
      sym_count <- sym_count + 1
    }
  }
  if (!is.na(dataset$MHCSF5[i])){
    if(dataset$MHCSF5[i]==4 | dataset$MHCSF5[i]==5){
      sym_count <- sym_count + 1
    }
  }
  if (!is.na(dataset$MHCSF6[i])){
    if(dataset$MHCSF6[i]==4 | dataset$MHCSF6[i]==5){
      sym_count <- sym_count + 1
    }
  }
  if (!is.na(dataset$MHCSF7[i])){
    if(dataset$MHCSF7[i]==4 | dataset$MHCSF7[i]==5){
      sym_count <- sym_count + 1
    }
  }
  if (!is.na(dataset$MHCSF8[i])){
    if(dataset$MHCSF8[i]==4 | dataset$MHCSF8[i]==5){
      sym_count <- sym_count + 1
    }
  }
  if (!is.na(dataset$MHCSF9[i])){
    if(dataset$MHCSF9[i]==4 | dataset$MHCSF9[i]==5){
      sym_count <- sym_count + 1
    }
  }
  if (!is.na(dataset$MHCSF10[i])){
    if(dataset$MHCSF10[i]==4 | dataset$MHCSF10[i]==5){
      sym_count <- sym_count + 1
    }
  }
  if (!is.na(dataset$MHCSF11[i])){
    if(dataset$MHCSF11[i]==4 | dataset$MHCSF11[i]==5){
      sym_count <- sym_count + 1
    }
  }
  if (!is.na(dataset$MHCSF12[i])){
    if(dataset$MHCSF12[i]==4 | dataset$MHCSF12[i]==5){
      sym_count <- sym_count + 1
    }
  }
  if (!is.na(dataset$MHCSF13[i])){
    if(dataset$MHCSF13[i]==4 | dataset$MHCSF13[i]==5){
      sym_count <- sym_count + 1
    }
  }
  if (!is.na(dataset$MHCSF14[i])){
    if(dataset$MHCSF14[i]==4 | dataset$MHCSF14[i]==5){
      sym_count <- sym_count + 1
    }
  }
  if(sym_count > 5){
    return(TRUE)
  }
  return(FALSE)
}

#count the number of "flourishing" respondents
flourishing_count <- 0
#create a boolean column in dataset for "flourishing" students 
dataset$flourishing <- FALSE #initializing data in the col

for(i in 1:rownum){
  #both functions must be true
  if(hedonic_wb(i) & positive_func(i)){
    dataset$flourishing[i] <- TRUE 
    flourishing_count <- flourishing_count + 1
  }
}

#percentage of respondents considered flourishing:
flourishing_percent <- flourishing_count/rownum

#################################################################################
# Depression and anxiety
#################################################################################

# Level of depression.
# 0-4 = no depression
# 5-9 = minimal depression
# 10-14 = mild depression
# 15-19 = major depression
# > 20 = severe major depression
Depression_total_mean <- mean(dataset$Depression_total, na.rm=TRUE) 
Depression_total_var <- var(dataset$Depression_total,na.rm = TRUE)
Depression_total_std <- sqrt(Depression_total_var)
Depression_total_rr <- response_rate(dataset$Depression_total)
Depression_total_box <- boxplot(dataset$Depression_total)

# Level of impairment from depression (scale: 0-6)
Depression_interference_mean <- mean(dataset$Depression_interference, na.rm=TRUE) 
Depression_interference_var <- var(dataset$Depression_interference, na.rm=TRUE)
Depression_interference_std <- sqrt(Depression_interference_var)
Depression_interference_rr <- response_rate(dataset$Depression_interference)
Depression_interference_box <- boxplot(dataset$Depression_interference)

# Level of anxiety 
# 5-9 = mild anxiety; 
# 10-14 = moderate anxiety; 
# 15-21 = severe anxiety
Anxiety_total_mean <- mean(dataset$Anxiety_total, na.rm=TRUE) 
Anxiety_total_var <- var(dataset$Anxiety_total,na.rm = TRUE)
Anxiety_total_std <- sqrt(Anxiety_total_var)
Anxiety_total_rr <- response_rate(dataset$Anxiety_total)
Anxiety_total_box <- boxplot(dataset$Anxiety_total)

# Level of impairment from anxiety (scale: 0-6)
Anxiety_interference_mean <- mean(dataset$Anxiety_interference, na.rm=TRUE) 
Anxiety_interference_var <- var(dataset$Anxiety_interference, na.rm = TRUE)
Anxiety_interference_std <- sqrt(Anxiety_interference_var)
Anxiety_interference_rr <- response_rate(dataset$Anxiety_interference)
Anxiety_interference_box <- boxplot(dataset$Anxiety_interference)

#################################################################################
# Non-suicidal self-injury and eating behavior
#################################################################################

# Frequency of non-suicidal self-injury as percentage of sample
# who engaged in at least one type of NSSI over the past year 
#?????

# Level of eating behavior impairment 
# (scale:0-45) >16 = At Risk
ED_total_mean <- mean(dataset$ED_total, na.rm=TRUE) 
ED_total_var <- var(dataset$ED_total, na.rm = TRUE)
ED_total_std <- sqrt(ED_total_var)
ED_total_rr <- response_rate(dataset$ED_total)

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
Overall_stress_mean <- mean(dataset$Overall_stress, na.rm=TRUE) 
Overall_stress_var <-var(dataset$Overall_stress, na.rm = TRUE)
Overall_stress_std <- sqrt(Overall_stress_var)
Overall_stress_rr <- response_rate(dataset$Overall_stress)

# Percieved Stress Scale
# 0-13: Low Stress
# 14-26: Moderate Stress
# 27-40: High Stress
Stress_total_total <- mean(dataset$Stress_total, na.rm=TRUE) 
Stress_total_var <- var(dataset$Stress_total, na.rm = TRUE)
Stress_total_std <- sqrt(Stress_total_var)
Stress_total_rr <- response_rate(dataset$Stress_total)

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
Stress_and_health_mean <- mean(dataset$Stress_and_health, na.rm=TRUE) 
Stress_and_health_var <- var(dataset$Stress_and_health, na.rm = TRUE)
Stress_and_health_std <- sqrt(Stress_and_health_var)
Stress_and_health_rr <- response_rate(dataset$Stress_and_health)

#Looking at Correlations and Covariances 
# using Pearson's Method and casewise deletions for missing data

# Between Overall Stress and Overall Mental Health
cor_overall_stress_overall_MH <- cor(dataset$Overall_stress, dataset$Overall_MH,
                                     use="complete.obs", method="pearson")
cov_overall_stress_overall_MH <- cov(dataset$Overall_stress, dataset$Overall_MH, 
                                     use="complete.obs", method="pearson")

# Between Overall Stress and Overall Physical Health
cor_overall_stress_overall_PH <- cor(dataset$Overall_stress, dataset$Overall_PH,
                                     use="complete.obs", method="pearson")
cov_overall_stress_overall_PH <- cov(dataset$Overall_stress, dataset$Overall_PH, 
                                     use="complete.obs", method="pearson")

# Between Overall Stress and Positive Emotions
cor_overall_stress_overall_PE_avg <- cor(dataset$Overall_stress, dataset$PE_avg,
                                         use="complete.obs", method="pearson")
cov_overall_stress_overall_PE_avg <- cov(dataset$Overall_stress, dataset$PE_avg, 
                                         use="complete.obs", method="pearson")


#################################################################################
# Coping with Stress
#################################################################################

#################################################################################
# Sleep
#################################################################################

# typical hours of sleep per night (open-ended)
Hours_sleep_min <- min(dataset$Hours_sleep, na.rm=TRUE) #minimum
Hours_sleep_max <- max(dataset$Hours_sleep, na.rm=TRUE) #maximum
Hours_sleep_mean <- mean(dataset$Hours_sleep, na.rm=TRUE) 
Hours_sleep_var <- var(dataset$Hours_sleep, na.rm = TRUE)
Hours_sleep_std <- sqrt(Hours_sleep_var)
Hours_sleep_rr <- response_rate(dataset$Hours_sleep)

#Sleep Quality (Scale: 1=very good - 4=very bad)
Sleep_quality_min <- min(dataset$Sleep_quality, na.rm=TRUE) #minimum
Sleep_quality_max <- max(dataset$Sleep_quality, na.rm=TRUE) #maximum
Sleep_quality_mean <- mean(dataset$Sleep_quality, na.rm=TRUE) 
Sleep_quality_var <- var(dataset$Sleep_quality, na.rm = TRUE)
Sleep_quality_std <- sqrt(Sleep_quality_var)
Sleep_quality_rr <- response_rate(dataset$Sleep_quality)
Sleep_quality_box <- boxplot(dataset$Sleep_quality)

#Sleep Hygiene (Scale: 14=very good - 70=very bad)
SHI_total_min <- min(dataset$SHI_total, na.rm=TRUE) #minimum
SHI_total_max <- max(dataset$SHI_total, na.rm=TRUE) #maximum
SHI_total_mean <- mean(dataset$SHI_total, na.rm=TRUE) 
SHI_total_var <- var(dataset$SHI_total, na.rm = TRUE)
SHI_total_std <- sqrt(SHI_total_var)
SHI_total_rr <- response_rate(dataset$SHI_total)
SHI_total_box <- boxplot(dataset$SHI_total)

# Between total Sleep Hygiene and sleep quality
cor_SHI_total_Sleep_quality <- cor(dataset$SHI_total, dataset$Sleep_quality, use="complete.obs", method="pearson")
cov_SHI_total_Sleep_quality <- cov(dataset$SHI_total, dataset$Sleep_quality, use="complete.obs", method="pearson")

# Between total Sleep Hygiene and feeling sleepy during the day
cor_SHI_total_Sleep_hygiene15 <- cor(dataset$SHI_total, dataset$Sleep_hygiene15, use="complete.obs", method="pearson")
cov_SHI_total_Sleep_hygiene15 <- cov(dataset$SHI_total, dataset$Sleep_hygiene15, use="complete.obs", method="pearson")

# Between total Sleep Hygiene and worry about sleep
cor_SHI_total_Sleep_hygiene16 <- cor(dataset$SHI_total, dataset$Sleep_hygiene16, use="complete.obs", method="pearson")
cov_SHI_total_Sleep_hygiene16 <- cov(dataset$SHI_total, dataset$Sleep_hygiene16, use="complete.obs", method="pearson")

# Between total Sleep Hygiene and feeling more moody now than before
cor_SHI_total_Sleep_hygiene17 <- cor(dataset$SHI_total, dataset$Sleep_hygiene17, use="complete.obs", method="pearson")
cov_SHI_total_Sleep_hygiene17 <- cov(dataset$SHI_total, dataset$Sleep_hygiene17, use="complete.obs", method="pearson")

# Between total Sleep Hygiene and feeling like it takes more effort to get things done
cor_SHI_total_Sleep_hygiene18 <- cor(dataset$SHI_total, dataset$Sleep_hygiene18, use="complete.obs", method="pearson")
cov_SHI_total_Sleep_hygiene18 <- cov(dataset$SHI_total, dataset$Sleep_hygiene18, use="complete.obs", method="pearson")

# Between total Sleep Hygiene and having more trouble paying attention and thinking than before
cor_SHI_total_Sleep_hygiene19 <- cor(dataset$SHI_total, dataset$Sleep_hygiene19, use="complete.obs", method="pearson")
cov_SHI_total_Sleep_hygiene19 <- cov(dataset$SHI_total, dataset$Sleep_hygiene19, use="complete.obs", method="pearson")

#################################################################################
# 
#################################################################################
