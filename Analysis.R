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
dataset <- read_excel("C:/Users/kates/Desktop/HWBI/HWBS/HWBS_STUDENTS_2017_condensed.xlsx")
rownum <- nrow(dataset) #total number of responses recorded

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
# At a glance
#################################################################################

#overall mental health  (scale:1-5)

Overall_MH_mean <- mean(dataset$Overall_MH, na.rm=TRUE)
Overall_MH_var <- var(dataset$Overall_MH, na.rm = TRUE)
Overall_MH_std <- std(dataset$Overall_MH)
Overall_MH_rr <- response_rate(dataset$Overall_MH)
Overall_MH_hist <- hist(na.omit(dataset$Overall_MH), breaks = c(0,1,2,3,4,5), xlab="Description of Mental Health",
     freq = FALSE, labels = c("Very Poor", "Poor", "Fair", "Good", "Excellent"), 
     main="Overall Mental Health")

analysis <- data.frame("Name"="Overall_MH","Mean"=Overall_MH_mean, 
                       "Std" = Overall_MH_std, "Response Rate"= Overall_MH_rr,
                       stringsAsFactors = FALSE)

#“I see myself as a person with mental illness” (scale:1-6)
MI_identity_R_mean <- mean(dataset$MI_identity_R, na.rm=TRUE) 
MI_identity_R_var <- var(dataset$MI_identity_R, na.rm = TRUE)
MI_identity_R_std <- std(dataset$MI_identity_R)
MI_identity_R_rr <- response_rate(dataset$MI_identity_R)
MI_identity_R_hist<- hist(na.omit(dataset$Overall_MH), breaks = c(0,1,2,3,4,5,6), 
     xlab="I see myself as a person with mental illness.",
     freq = FALSE, labels = c("Strongly Disagree", "Disagree", "Somewhat Disagree",
                              "Somewhat Agree", "Agree", "Strongly Agree"), 
     main="Mental Illness Identity")

analysis <- rbind(analysis, list("MI_identity_R", MI_identity_R_mean,
                                 MI_identity_R_std, MI_identity_R_rr))

#overall physical health (scale: 1-5)

Overall_PH_mean <- mean(dataset$Overall_PH, na.rm=TRUE) 
Overall_PH_var <- var(dataset$Overall_PH, na.rm = TRUE)
Overall_PH_std <- sqrt(Overall_PH_var)
Overall_PH_rr <- response_rate(dataset$Overall_PH)
Overall_PH_hist <- hist(na.omit(dataset$Overall_PH), breaks = c(0,1,2,3,4,5), xlab="Description of Physical Health",
                          freq = FALSE, labels = c("Very Poor", "Poor", "Fair", "Good", "Excellent"), 
                          main="Overall Physical Health", ylim = range(0,0.5))

analysis <- rbind(analysis, list("Overall_PH", Overall_PH_mean, 
                                 Overall_PH_std, Overall_PH_rr))

#overall stress (scale: 1-10)
Overall_stress_mean <- mean(dataset$Overall_stress, na.rm=TRUE) 
Overall_stress_var <- var(dataset$Overall_stress, na.rm = TRUE)
Overall_stress_std <- sqrt(Overall_stress_var)
Overall_stress_rr <- response_rate(dataset$Overall_stress)
Overall_stress_hist <- hist(na.omit(dataset$Overall_stress), breaks = c(-.1,1,2,3,4,5,6,7,8,9,10.05), xlab="Average Stress Level in the Past Month",
     freq = FALSE, labels = c("No \nStress","","","","","","","","","Very\nHigh\n Stress"), 
     main="Overall Stress")


analysis <- rbind(analysis, list("Overall_stress", Overall_stress_mean, 
                                 Overall_stress_std, Overall_stress_rr))

outlierReplace = function(dataframe, cols, rows, newValue = NA) {
  if (any(rows)) {
    set(dataframe, rows, cols, newValue)
  }
}

#hours per week exercising (open-ended)
Hours_exercising_mean <- mean(dataset$Hours_exercising, na.rm=TRUE) #including outliers
Hours_exercising_var <- var(dataset$Hours_exercising, na.rm = TRUE)
Hours_exercising_std <- sqrt(Hours_exercising_var)
Hours_exercising_box <- boxplot(dataset$Hours_exercising, main="Typical Hours Spent Exercising per Week") #Boxplot with outliers
Hours_exercising_rr <- response_rate(dataset$Hours_exercising)
Hours_exercising_summary <- summary(dataset$Hours_exercising)

analysis <- rbind(analysis, list("Hours_exercising", Hours_exercising_mean, 
                                 Hours_exercising_std, Hours_exercising_rr))

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

analysis <- rbind(analysis, list("Hours_exercising_no_extreme_outliers", Hours_exercising_no_extreme_outliers_mean, 
                                 Hours_exercising_no_extreme_outliers_std, Hours_exercising_no_extreme_outliers_rr))

# typical hours of sleep per night (open-ended)
Hours_sleep_summary <- summary(dataset$Hours_sleep)
Hours_sleep_mean <- mean(dataset$Hours_sleep, na.rm=TRUE) 
Hours_sleep_var <- var(dataset$Hours_sleep, na.rm = TRUE)
Hours_sleep_std <- sqrt(Hours_sleep_var)
Hours_sleep_box <- boxplot(dataset$Hours_sleep, main="Typical Hours of Sleep per Night")
Hours_sleep_rr <- response_rate(dataset$Hours_sleep)

analysis <- rbind(analysis, list("Hours_sleep", Hours_sleep_mean, 
                                 Hours_sleep_std, Hours_sleep_rr))
#how respondents feel the campus environment at Hendrix 
#impacts students’ mental health (scale: -3 to +3)
HDX_MH_impact_mean <- mean(dataset$HDX_MH_impact, na.rm=TRUE) 
HDX_MH_impact_var <- var(dataset$HDX_MH_impact, na.rm = TRUE)
HDX_MH_impact_std <- sqrt(HDX_MH_impact_var)
HDX_MH_impact_rr <- response_rate(dataset$HDX_MH_impact)
HDX_MH_impact_box <- boxplot(dataset$HDX_MH_impact, ylab="Hendrix Mental Health Impact", pars = list(par(mar=c(5,4,4,2)+0.1))) #the responses here are very symetrical 
HDX_MH_impact_hist <- hist(na.omit(dataset$HDX_MH_impact), breaks = c(-4,-3, -2, -1, 0, 1, 2, 3), 
                     xlab="Campus Environment Impact on Mental and Emotional Health",
                     freq = FALSE, labels = c("Very \n Negative","","","","","","Very \n Positive"), 
                     main="Hendrix Impact on Student Mental Health")

analysis <- rbind(analysis, list("HDX_MH_impact", HDX_MH_impact_mean, 
                                 HDX_MH_impact_std, HDX_MH_impact_rr))
#################################################################################
# Positive mental health
#################################################################################

# Mental Health Continuum – Short Form (scale:0-70)
MHCSF_total_mean <- mean(dataset$MHCSF_total, na.rm=TRUE) 
MHCSF_total_var <- var(dataset$MHCSF_total,na.rm = TRUE)
MHCSF_total_std <- std(dataset$MHCSF_total)
MHCSF_total_rr <- response_rate(dataset$MHCSF_total)
MHCSF_total_box <- boxplot(dataset$MHCSF_total, ylab= "Total Score", main="Mental Health Continuum Short Form")

analysis <- rbind(analysis, list("MHCSF_total", MHCSF_total_mean, 
                                 MHCSF_total_std, MHCSF_total_rr))

#Level of positive emotions (scale 0-4)
PE_avg_mean <- mean(dataset$PE_avg, na.rm=TRUE) 
PE_avg_var <- var(dataset$PE_avg, na.rm = TRUE)
PE_avg_std <- std(dataset$PE_avg)
PE_avg_rr <- response_rate(dataset$PE_avg)
PE_avg_box <- boxplot(dataset$PE_avg, main="Average Positive Emotion Rating")

analysis <- rbind(analysis, list("PE_avg", PE_avg_mean, 
                                 PE_avg_std, PE_avg_rr))

# Level of resilience (scale 1-5)
Resilience_avg_mean <- mean(dataset$Resilience_avg, na.rm=TRUE) 
Resilience_avg_var <- var(dataset$Resilience_avg, na.rm = TRUE)
Resilience_avg_std <- std(dataset$Resilience_avg)
Resilience_avg_rr <- response_rate(dataset$Resilience_avg)
Resilience_avg_box <- boxplot(dataset$Resilience_avg, main="Average of Resilience Score")

analysis <- rbind(analysis, list("Resilience_avg", Resilience_avg_mean, 
                                 Resilience_avg_std, Resilience_avg_rr))


# Level of satisfaction with personal 
#relationships at HDX (scale 1-5)
Relationship_satis_mean <- mean(dataset$Relationship_satis, na.rm=TRUE) 
Relationship_satis_var <- var(dataset$Relationship_satis, na.rm = TRUE)
Relationship_satis_std <- std(dataset$Relationship_satis)
Relationship_satis_rr <- response_rate(dataset$Relationship_satis)
Relationship_satis_box <- boxplot(dataset$Relationship_satis)
Relationship_satis_hist<- hist(na.omit(dataset$Relationship_satis), breaks = c(0,1,2,3,4,5), 
     xlab="Satisfaction with Relationships with People at Hendrix",
     freq = FALSE, labels = c("Strongly \n Disagree","","","", "Strongly \n Agree"), ylim = range(0,0.4),
     main="Relationship Satisfaction")

analysis <- rbind(analysis, list("Relationship_satis", Relationship_satis_mean, 
                                 Relationship_satis_std, Relationship_satis_rr))


# Level of belongingness (scale 9-54)
Belonging_total_mean <- mean(dataset$Belonging_total, na.rm=TRUE) 
Belonging_total_var <- var(dataset$Belonging_total, na.rm = TRUE)
Belonging_total_std <- std(dataset$Belonging_total)
Belonging_total_rr <- response_rate(dataset$Belonging_total)
Belonging_total_box <- boxplot(dataset$Belonging_total, main="Feelings of Belongingness")

analysis <- rbind(analysis, list("Belonging_total", Belonging_total_mean, 
                                 Belonging_total_std, Belonging_total_rr))


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
Depression_total_std <- std(dataset$Depression_total)
Depression_total_rr <- response_rate(dataset$Depression_total)
Depression_total_box <- boxplot(dataset$Depression_total, main="Depression Total")

analysis <- rbind(analysis, list("Depression_total", Depression_total_mean, 
                                 Depression_total_std, Depression_total_rr))


# Level of impairment from depression (scale: 0-6)
Depression_interference_mean <- mean(dataset$Depression_interference, na.rm=TRUE) 
Depression_interference_var <- var(dataset$Depression_interference, na.rm=TRUE)
Depression_interference_std <- std(dataset$Depression_interference)
Depression_interference_rr <- response_rate(dataset$Depression_interference)
Depression_interference_box <- boxplot(dataset$Depression_interference)
Depression_interference_hist <- hist(na.omit(dataset$Depression_interference), breaks = c(-1,0,1,2,3,4,5,6), 
      xlab="Difficulty Working and Socializing due to Depression",
      freq = FALSE, labels = c("Not Difficult","","","","","", "Very Difficult"), 
      main="Depression Interference")

analysis <- rbind(analysis, list("Depression_interference", Depression_interference_mean, 
                                 Depression_interference_std, Depression_interference_rr))


# Level of anxiety 
# 5-9 = mild anxiety; 
# 10-14 = moderate anxiety; 
# 15-21 = severe anxiety
Anxiety_total_mean <- mean(dataset$Anxiety_total, na.rm=TRUE) 
Anxiety_total_var <- var(dataset$Anxiety_total,na.rm = TRUE)
Anxiety_total_std <- std(dataset$Anxiety_total)
Anxiety_total_rr <- response_rate(dataset$Anxiety_total)
Anxiety_total_box <- boxplot(dataset$Anxiety_total, main="Total Anxiety")

analysis <- rbind(analysis, list("Anxiety_total", Anxiety_total_mean, 
                                 Anxiety_total_std, Anxiety_total_rr))


# Level of impairment from anxiety (scale: 0-6)
Anxiety_interference_mean <- mean(dataset$Anxiety_interference, na.rm=TRUE) 
Anxiety_interference_var <- var(dataset$Anxiety_interference, na.rm = TRUE)
Anxiety_interference_std <- std(dataset$Anxiety_interference)
Anxiety_interference_rr <- response_rate(dataset$Anxiety_interference)
Anxiety_interference_box <- boxplot(dataset$Anxiety_interference)
Anxiety_interference_hist <- hist(na.omit(dataset$Anxiety_interference), breaks = c(-1,0,1,2,3,4,5,6), 
     xlab="Difficulty Working and Socializing due to Anxiety",
     freq = FALSE, labels = c("Not Difficult","","","","","", "Very Difficult"), 
     main="Anxiety Interference")

analysis <- rbind(analysis, list("Anxiety_interference", Anxiety_interference_mean, 
                                 Anxiety_interference_std, Anxiety_interference_rr))


#################################################################################
# Non-suicidal self-injury and eating behavior
#################################################################################

# Frequency of non-suicidal self-injury as percentage of sample
# who engaged in at least one type of NSSI over the past year 
NSSI_total_tbl <- table(na.omit(dataset$NSSI_total))
sum(NSSI_total_tbl[names(NSSI_total_tbl)>0])/sum(NSSI_total_tbl)

# Level of eating behavior impairment 
# (scale:0-45) >16 = At Risk
ED_total_mean <- mean(dataset$ED_total, na.rm=TRUE) 
ED_total_var <- var(dataset$ED_total, na.rm = TRUE)
ED_total_std <- sqrt(ED_total_var)
ED_total_rr <- response_rate(dataset$ED_total)
ED_total_hist <- hist(na.omit(dataset$ED_total), breaks = c(-1,5, 10, 15, 20, 25, 30, 35, 40, 45), 
                      xlab="Eating Behaviour Impariment Level",
                      freq = TRUE, labels = c("","","","At \n Risk","","","","","",""), ylim=range(0,200), 
                      main="Eating Disorder")


analysis <- rbind(analysis, list("ED_total", ED_total_mean, 
                                 ED_total_std, ED_total_rr))


#################################################################################
# Stress
#################################################################################

# overall stress (scale: 1-10)
Overall_stress_mean <- mean(dataset$Overall_stress, na.rm=TRUE) 
Overall_stress_var <-var(dataset$Overall_stress, na.rm = TRUE)
Overall_stress_std <- sqrt(Overall_stress_var)
Overall_stress_rr <- response_rate(dataset$Overall_stress)

analysis <- rbind(analysis, list("Overall_stress", Overall_stress_mean, 
                                 Overall_stress_std, Overall_stress_rr))


# Percieved Stress Scale
# 0-13: Low Stress
# 14-26: Moderate Stress
# 27-40: High Stress
Stress_total_mean <- mean(dataset$Stress_total, na.rm=TRUE) 
Stress_total_var <- var(dataset$Stress_total, na.rm = TRUE)
Stress_total_std <- sqrt(Stress_total_var)
Stress_total_rr <- response_rate(dataset$Stress_total)

analysis <- rbind(analysis, list("Stress_total", Stress_total_mean, 
                                 Stress_total_std, Stress_total_rr))


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

analysis <- rbind(analysis, list("Stress_and_health", Stress_and_health_mean, 
                                 Stress_and_health_std, Stress_and_health_rr))

# To what extent do you think stress negatively affects your personal happiness
# Scale (0-6)
Stress_and_happiness_mean <- mean(dataset$Stress_and_happiness, na.rm=TRUE) 
Stress_and_happiness_var <- var(dataset$Stress_and_happiness, na.rm = TRUE)
Stress_and_happiness_std <- sqrt(Stress_and_happiness_var)
Stress_and_happiness_rr <- response_rate(dataset$Stress_and_happiness)
Stress_and_happiness_box <- boxplot(dataset$Stress_and_happiness)

analysis <- rbind(analysis, list("Stress_and_happiness", Stress_and_happiness_mean, 
                                 Stress_and_happiness_std, Stress_and_happiness_rr))

# To what extent do you think stress negatively affects your personal Mental Health
# Scale (0-6)
Stress_and_MH_mean <- mean(dataset$Stress_and_MH, na.rm=TRUE) 
Stress_and_MH_var <- var(dataset$Stress_and_MH, na.rm = TRUE)
Stress_and_MH_std <- sqrt(Stress_and_MH_var)
Stress_and_MH_rr <- response_rate(dataset$Stress_and_MH)
Stress_and_MH_box <- boxplot(dataset$Stress_and_MH)

analysis <- rbind(analysis, list("Stress_and_MH", Stress_and_MH_mean, 
                                 Stress_and_MH_std, Stress_and_MH_rr))

# To what extent do you think stress negatively affects your personal Physical Health
# Scale (0-6)
Stress_and_PH_mean <- mean(dataset$Stress_and_PH, na.rm=TRUE) 
Stress_and_PH_var <- var(dataset$Stress_and_PH, na.rm = TRUE)
Stress_and_PH_std <- sqrt(Stress_and_PH_var)
Stress_and_PH_rr <- response_rate(dataset$Stress_and_PH)
Stress_and_PH_box <- boxplot(dataset$Stress_and_PH)

analysis <- rbind(analysis, list("Stress_and_PH", Stress_and_PH_mean, 
                                 Stress_and_PH_std, Stress_and_PH_rr))

#Looking at Correlations and Covariances 
# using Pearson's Method and casewise deletions for missing data

# Between Overall Stress and Overall Mental Health
overall_stress_overall_MH_cor <- cor(dataset$Overall_stress, dataset$Overall_MH,
                                     use="complete.obs", method="pearson")
overall_stress_overall_MH_cov <- cov(dataset$Overall_stress, dataset$Overall_MH, 
                                     use="complete.obs", method="pearson")

corcov <- data.frame("Name1"="Overall_stress", "Name2" = "Overall_MH",
                     "correlation" = overall_stress_overall_MH_cor,
                     "covariance" = overall_stress_overall_MH_cov,
                       stringsAsFactors = FALSE)

# Between Overall Stress and Overall Physical Health
overall_stress_overall_PH_cor <- cor(dataset$Overall_stress, dataset$Overall_PH,
                                     use="complete.obs", method="pearson")
overall_stress_overall_PH_cov <- cov(dataset$Overall_stress, dataset$Overall_PH, 
                                     use="complete.obs", method="pearson")

corcov <- rbind(corcov, list("Overall_stress", "Overall_PH", 
                             overall_stress_overall_PH_cor, overall_stress_overall_PH_cov))


# Between Overall Stress and Positive Emotions
overall_stress_overall_PE_avg_cor <- cor(dataset$Overall_stress, dataset$PE_avg,
                                         use="complete.obs", method="pearson")
overall_stress_overall_PE_avg_cov <- cov(dataset$Overall_stress, dataset$PE_avg, 
                                         use="complete.obs", method="pearson")
corcov <- rbind(corcov, list("Overall_stress", "PE_avg",
                             overall_stress_overall_PE_avg_cor,overall_stress_overall_PE_avg_cov ))

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
#percentage of people who meditate:
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


# To what extent do you think you have been doing a good job of managing your 
# stress level over the past month? Scale (0-6)
Managing_stress_mean <- mean(dataset$Managing_stress, na.rm=TRUE) 
Managing_stress_var <- var(dataset$Managing_stress, na.rm = TRUE)
Managing_stress_std <- sqrt(Managing_stress_var)
Managing_stress_rr <- response_rate(dataset$Managing_stress)
Managing_stress_box <- boxplot(dataset$Managing_stress, main="Managing Stress")

analysis <- rbind(analysis, list("Managing_stress", Managing_stress_mean, 
                                 Managing_stress_std, Managing_stress_rr))

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

analysis <- rbind(analysis, list("Hours_sleep", Hours_sleep_mean, 
                                 Hours_sleep_std, Hours_sleep_rr))


#Sleep Quality (Scale: 1=very good - 4=very bad)
Sleep_quality_R_min <- min(dataset$Sleep_quality_R, na.rm=TRUE) #minimum
Sleep_quality_R_max <- max(dataset$Sleep_quality_R, na.rm=TRUE) #maximum
Sleep_quality_R_mean <- mean(dataset$Sleep_quality_R, na.rm=TRUE) 
Sleep_quality_R_var <- var(dataset$Sleep_quality_R, na.rm = TRUE)
Sleep_quality_R_std <- sqrt(Sleep_quality_R_var)
Sleep_quality_R_rr <- response_rate(dataset$Sleep_quality)
Sleep_quality_R_box <- boxplot(dataset$Sleep_quality_R, main="Overall Sleep Quality")

analysis <- rbind(analysis, list("Sleep_quality_R", Sleep_quality_R_mean, 
                                 Sleep_quality_R_std, Sleep_quality_R_rr))


#Sleep Hygiene (Scale: 14=very good - 70=very bad)
SHI_total_min <- min(dataset$SHI_total, na.rm=TRUE) #minimum
SHI_total_max <- max(dataset$SHI_total, na.rm=TRUE) #maximum
SHI_total_mean <- mean(dataset$SHI_total, na.rm=TRUE) 
SHI_total_var <- var(dataset$SHI_total, na.rm = TRUE)
SHI_total_std <- sqrt(SHI_total_var)
SHI_total_rr <- response_rate(dataset$SHI_total)
SHI_total_box <- boxplot(dataset$SHI_total, main="Sleep Hygiene Index Totals")

analysis <- rbind(analysis, list("SHI_total", SHI_total_mean, 
                                 SHI_total_std, SHI_total_rr))


# Between total Sleep Hygiene and sleep quality
cor_SHI_total_Sleep_quality <- cor(dataset$SHI_total, dataset$Sleep_quality, use="complete.obs", method="pearson")
cov_SHI_total_Sleep_quality <- cov(dataset$SHI_total, dataset$Sleep_quality, use="complete.obs", method="pearson")
corcov <- rbind(corcov, list("SHI_total", "Sleep_quality",cor_SHI_total_Sleep_quality ,cov_SHI_total_Sleep_quality ))

# Between total Sleep Hygiene and feeling sleepy during the day
cor_SHI_total_Sleep_hygiene15 <- cor(dataset$SHI_total, dataset$Sleep_hygiene15, use="complete.obs", method="pearson")
cov_SHI_total_Sleep_hygiene15 <- cov(dataset$SHI_total, dataset$Sleep_hygiene15, use="complete.obs", method="pearson")
corcov <- rbind(corcov, list("SHI_total", "Sleep_hygiene15", cor_SHI_total_Sleep_hygiene15, cov_SHI_total_Sleep_hygiene15))

# Between total Sleep Hygiene and worry about sleep
cor_SHI_total_Sleep_hygiene16 <- cor(dataset$SHI_total, dataset$Sleep_hygiene16, use="complete.obs", method="pearson")
cov_SHI_total_Sleep_hygiene16 <- cov(dataset$SHI_total, dataset$Sleep_hygiene16, use="complete.obs", method="pearson")
corcov <- rbind(corcov, list("SHI_total", "Sleep_hygiene16",cor_SHI_total_Sleep_hygiene16 ,cov_SHI_total_Sleep_hygiene16 ))

# Between total Sleep Hygiene and feeling more moody now than before
cor_SHI_total_Sleep_hygiene17 <- cor(dataset$SHI_total, dataset$Sleep_hygiene17, use="complete.obs", method="pearson")
cov_SHI_total_Sleep_hygiene17 <- cov(dataset$SHI_total, dataset$Sleep_hygiene17, use="complete.obs", method="pearson")
corcov <- rbind(corcov, list("SHI_total", "Sleep_hygiene17", cor_SHI_total_Sleep_hygiene17,cov_SHI_total_Sleep_hygiene17 ))

# Between total Sleep Hygiene and feeling like it takes more effort to get things done
cor_SHI_total_Sleep_hygiene18 <- cor(dataset$SHI_total, dataset$Sleep_hygiene18, use="complete.obs", method="pearson")
cov_SHI_total_Sleep_hygiene18 <- cov(dataset$SHI_total, dataset$Sleep_hygiene18, use="complete.obs", method="pearson")
corcov <- rbind(corcov, list("SHI_total", "Sleep_hygiene18",cor_SHI_total_Sleep_hygiene18 , cov_SHI_total_Sleep_hygiene18))

# Between total Sleep Hygiene and having more trouble paying attention and thinking than before
cor_SHI_total_Sleep_hygiene19 <- cor(dataset$SHI_total, dataset$Sleep_hygiene19, use="complete.obs", method="pearson")
cov_SHI_total_Sleep_hygiene19 <- cov(dataset$SHI_total, dataset$Sleep_hygiene19, use="complete.obs", method="pearson")
corcov <- rbind(corcov, list("SHI_total", "Sleep_hygiene19",cor_SHI_total_Sleep_hygiene19 ,cov_SHI_total_Sleep_hygiene19 ))

#################################################################################
# General Mental Health Knowledge
#################################################################################

#Recognizing someone in distress (1 = Strongly Agree - 6 = Strongly Disagree)
Recognize_distress_mean <- mean(dataset$Recognize_distress, na.rm=TRUE) 
Recognize_distress_var <- var(dataset$Recognize_distress, na.rm = TRUE)
Recognize_distress_std <- std(dataset$Recognize_distress)
Recognize_distress_rr <- response_rate(dataset$Recognize_distress)
Recognize_distress_box  <- boxplot(dataset$Recognize_distress)

analysis <- rbind(analysis, list("Recognize_distress", Recognize_distress_mean, 
                                 Recognize_distress_std, Recognize_distress_rr))


#I feel confident in helping someone with a mental health problem.
#(1 = Strongly Agree - 6 = Strongly Disagree)
Helping_confidence_mean <- mean(dataset$Helping_confidence, na.rm=TRUE) 
Helping_confidence_var <- var(dataset$Helping_confidence, na.rm = TRUE)
Helping_confidence_std <- std(dataset$Helping_confidence)
Helping_confidence_rr <- response_rate(dataset$Helping_confidence)
Helping_confidence_box <- boxplot(dataset$Helping_confidence)

analysis <- rbind(analysis, list("Helping_confidence", Helping_confidence_mean, 
                                 Helping_confidence_std, Helping_confidence_rr))


#Knowing where to go for mental health help
#(1 = Strongly Agree - 6 = Strongly Disagree)
MH_know_where_to_go_mean <- mean(dataset$MH_know_where_to_go, na.rm=TRUE) 
MH_know_where_to_go_var <- var(dataset$MH_know_where_to_go, na.rm = TRUE)
MH_know_where_to_go_std <- std(dataset$MH_know_where_to_go)
MH_know_where_to_go_rr <- response_rate(dataset$MH_know_where_to_go)
MH_know_where_to_go_box <- boxplot(dataset$MH_know_where_to_go)

analysis <- rbind(analysis, list("MH_know_where_to_go", MH_know_where_to_go_mean, 
                                 MH_know_where_to_go_std, MH_know_where_to_go_rr))


##Knowing where to go for physical health help
#(1 = Strongly Agree - 6 = Strongly Disagree)
PH_know_where_to_go_mean <- mean(dataset$PH_know_where_to_go, na.rm=TRUE) 
PH_know_where_to_go_var <- var(dataset$PH_know_where_to_go, na.rm = TRUE)
PH_know_where_to_go_std <- std(dataset$PH_know_where_to_go)
PH_know_where_to_go_rr <- response_rate(dataset$PH_know_where_to_go)
PH_know_where_to_go_box <- boxplot(dataset$PH_know_where_to_go)

analysis <- rbind(analysis, list("PH_know_where_to_go", PH_know_where_to_go_mean, 
                                 PH_know_where_to_go_std, PH_know_where_to_go_rr))


#Percentage of students who have participated in MH training
MH_training_freq <- freq_bool(na.omit(dataset$MH_training))

#################################################################################
# Use of Health Services
#################################################################################

# Frequency who have vs. have not received counseling 
Ever_counseling_freq <- freq_bool(dataset$Ever_counseling)

#	Frequency who are vs. are not currently receiving counseling 
Current_counseling_freq <-freq_bool(dataset$Current_counseling)

#	Frequency who have vs. have not used mental health services or counseling on campus 
Used_campus_MH_services_freq <-freq_bool(dataset$Used_campus_MH_services)

#	Frequency  who have vs. have not used physical health services on campus 
Used_campus_PH_services_freq <-freq_bool(dataset$Used_campus_PH_services)

# Perception of of MH/counciling services availible at HDX
Availability_MH_services_tbl <- table(na.omit(dataset$Availability_MH_services))
# Mostly negative opinions - 1
Availability_MH_services_neg <- sum(Availability_MH_services_tbl[names(Availability_MH_services_tbl)==1])/sum(Availability_MH_services_tbl)
# Mix of negative and positive opinions - 2
Availability_MH_services_both <- sum(Availability_MH_services_tbl[names(Availability_MH_services_tbl)==2])/sum(Availability_MH_services_tbl)
# Mostly positive opinions - 3
Availability_MH_services_pos <- sum(Availability_MH_services_tbl[names(Availability_MH_services_tbl)==3])/sum(Availability_MH_services_tbl)
# I have no opinions and haven’t heard others’ opinions - 4
Availability_MH_services_none <- sum(Availability_MH_services_tbl[names(Availability_MH_services_tbl)==4])/sum(Availability_MH_services_tbl)
# Ok histogram of results...
Availability_MH_services_hist <- hist(dataset$Availability_MH_services, breaks = c(0,1,2,3,4), xlab="Availability of MH services")

# Perception of of PH/counciling services availible at HDX
Availability_PH_services_tbl <- table(na.omit(dataset$Availability_PH_services))
# Mostly negative opinions - 1
Availability_PH_services_neg <- sum(Availability_PH_services_tbl[names(Availability_PH_services_tbl)==1])/sum(Availability_PH_services_tbl)
# Mix of negative and positive opinions - 2
Availability_PH_services_both <- sum(Availability_PH_services_tbl[names(Availability_PH_services_tbl)==2])/sum(Availability_PH_services_tbl)
# Mostly positive opinions - 3
Availability_PH_services_pos <- sum(Availability_PH_services_tbl[names(Availability_PH_services_tbl)==3])/sum(Availability_PH_services_tbl)
# I have no opinions and haven’t heard others’ opinions - 4
Availability_PH_services_none <- sum(Availability_PH_services_tbl[names(Availability_PH_services_tbl)==4])/sum(Availability_PH_services_tbl)
# Ok histogram of results...
Availability_PH_services_hist <- hist(dataset$Availability_PH_services, breaks = c(0,1,2,3,4), xlab="Availability of PH services")

# In the last year, I needed MH help (1=stongly disagree - 6 = strongly agree)
MH_needs_mean <- mean(dataset$MH_needs, na.rm=TRUE) 
MH_needs_var <- var(dataset$MH_needs, na.rm = TRUE)
MH_needs_std <- std(dataset$MH_needs)
MH_needs_rr <- response_rate(dataset$MH_needs)
MH_needs_box <- boxplot(dataset$MH_needs)
MH_needs_hist <- hist(dataset$MH_needs, breaks = c(0,1,2,3,4,5,6), main="Mental Health Needs", labels = c("Strongly \n Disagree","","",""
                                                                                         ,"","Strongly  Agree"), freq = FALSE)

analysis <- rbind(analysis, list("MH_needs", MH_needs_mean, 
                                 MH_needs_std, MH_needs_rr))


# In the last year, I felt my MH needs were met (1=stongly disagree - 6 = strongly agree)
MH_needs_met_mean <- mean(dataset$MH_needs_met, na.rm=TRUE) 
MH_needs_met_var <-var(dataset$MH_needs_met, na.rm = TRUE)
MH_needs_met_std <-std(dataset$MH_needs_met)
MH_needs_met_rr <-response_rate(dataset$MH_needs_met)
MH_needs_met_box <-boxplot(dataset$MH_needs_met)

analysis <- rbind(analysis, list("MH_needs_met", MH_needs_met_mean, 
                                 MH_needs_met_std, MH_needs_met_rr))


# factors that have caused respondents to receive fewer mental health 
# services than they would have liked to receive
Barrier_availableappt_freq <- freq_bool(dataset$Barrier_availableappt)
Barrier_dontwantservices_freq <- freq_bool(dataset$Barrier_dontwantservices)
Barrier_finances_freq <- freq_bool(dataset$Barrier_finances)
#Barrier_NA_freq <- freq_bool(dataset$Barrier_NA) #Not using NA now
Barrier_none_freq <- freq_bool(dataset$Barrier_none)
Barrier_notsurewheretogo_freq <- freq_bool(dataset$Barrier_notsurewheretogo)
Barrier_time_freq <- freq_bool(dataset$Barrier_time)

Barriers_all <- data.frame(Barrier_availableappt_freq, Barrier_dontwantservices_freq,
                           Barrier_finances_freq, Barrier_none_freq,
                           Barrier_notsurewheretogo_freq, Barrier_time_freq)
Barriers_all_sorted <- sort(Barriers_all, decreasing = TRUE)
#bad bar chart
Barriers_all_sorted_bar <- barplot(as.numeric(Barriers_all_sorted[1,]), 
                                   names.arg=c(colnames(Barriers_all_sorted)), las=2)

#################################################################################
# Mental Health Stigma
#################################################################################

# General MH stigma (scale: 3-18)
Stigma_total_mean <- mean(dataset$Stigma_total, na.rm=TRUE) 
Stigma_total_var <- var(dataset$Stigma_total, na.rm = TRUE)
Stigma_total_std <- std(dataset$Stigma_total)
Stigma_total_rr <- response_rate(dataset$Stigma_total)
Stigma_total_box <- boxplot(dataset$Stigma_total, ylab="Total Ratings of Mental Health Stigma")

analysis <- rbind(analysis, list("Stigma_total", Stigma_total_mean, 
                                 Stigma_total_std, Stigma_total_rr))


# The extent to which an individual conceals their mental illness (scale: 3-18)
Concealment_total_mean <- mean(dataset$Concealment_total, na.rm=TRUE) 
Concealment_total_var <- var(dataset$Concealment_total, na.rm = TRUE)
Concealment_total_std <- std(dataset$Concealment_total)
Concealment_total_rr <- response_rate(dataset$Concealment_total)
Concealment_total_box <- boxplot(dataset$Concealment_total)

analysis <- rbind(analysis, list("Concealment_total", Concealment_total_mean, 
                                 Concealment_total_std, Concealment_total_rr))


#################################################################################
# MH and Academic Performance
#################################################################################

#In the past month, how many days have you felt that emotional or mental health 
#difficulties have hurt your academic performance?
#None; 0 days - 1
#1-2 days - 2
#3-5 days - 3
#6 or more days - 4
MH_academic_impact_tbl <- table(na.omit(dataset$MH_academic_impact)) 
par(mar=c(5,6,4,1)+.1)
MH_academic_impact_hist <- hist(dataset$MH_academic_impact,  breaks = c(0,1,2,3,4),
                                freq = FALSE, labels = c("0", "1-2", "3-5", ">5"), 
                                xlab = "Days of Hurt Academic Performance", ylim = range(0,0.35),
                                main = "MH Affecting Academic Performance in Past Month")

PH_academic_impact_tbl <- table(dataset$PH_academic_impact)
Eating_academic_impact_tbl <- table(dataset$Eating_academic_impact)
Substance_academic_impact_tbl <- (dataset$Substance_academic_impact)
Anxiety_academic_impact_tbl <- table(dataset$Anxiety_academic_impact)
Depression_academic_impact_tbl <- table(dataset$Depression_academic_impact)
Extracurricular_academic_impact_tbl <- table(dataset$Extracurricular_academic_impact)
HelpingOthers_academic_impact_tbl <- table(dataset$HelpingOthers_academic_impact)
Relationships_academic_impact_tbl <- table(dataset$Relationships_academic_impact)
Discrimination_academic_impact_tbl <- table(dataset$Discrimination_academic_impact)

#################################################################################
# Interrelationships
#################################################################################

# Between Anxiety and Depression
Anxiety_total_Depression_total_cor <- cor(dataset$Depression_total, dataset$Anxiety_total, use="complete.obs", method="pearson")
Anxiety_total_Depression_total_cov <- cov(dataset$Depression_total, dataset$Anxiety_total, use="complete.obs", method="pearson")
corcov <- rbind(corcov, list("Depression_total", "Anxiety_total", Anxiety_total_Depression_total_cor,Anxiety_total_Depression_total_cov ))

#Between Hours of Sleep and Depression
Depression_total_Hours_sleep_cor <- cor(dataset$Depression_total, dataset$Hours_sleep, use="complete.obs", method="pearson")
Depression_total_Hours_sleep_cov <- cov(dataset$Depression_total, dataset$Hours_sleep, use="complete.obs", method="pearson")
corcov <- rbind(corcov, list("Hours_sleep", "Depression_total", Depression_total_Hours_sleep_cor, Depression_total_Hours_sleep_cov))

#Between Hours of Sleep and anxiety
Anxiety_total_Hours_sleep_cor <- cor(dataset$Anxiety_total, dataset$Hours_sleep, use="complete.obs", method="pearson")
Anxiety_total_Hours_sleep_cov <- cov(dataset$Anxiety_total, dataset$Hours_sleep, use="complete.obs", method="pearson")
corcov <- rbind(corcov, list("Hours_sleep", "Anxiety_total", Anxiety_total_Hours_sleep_cor, Anxiety_total_Hours_sleep_cov))

#Between Hours of Sleep and stress
Overall_stress_Hours_sleep_cor <- cor(dataset$Overall_stress, dataset$Hours_sleep, use="complete.obs", method="pearson")
Overall_stress_Hours_sleep_cov <- cov(dataset$Overall_stress, dataset$Hours_sleep, use="complete.obs", method="pearson")
corcov <- rbind(corcov, list("Hours_sleep", "Overall_stress",Overall_stress_Hours_sleep_cor , Overall_stress_Hours_sleep_cov))

#Between Hours of Sleep and Mental Heath Continuum score
MHCSF_total_Hours_sleep_cor <- cor(dataset$MHCSF_total, dataset$Hours_sleep, use="complete.obs", method="pearson")
MHCSF_total_Hours_sleep_cov <- cov(dataset$MHCSF_total, dataset$Hours_sleep, use="complete.obs", method="pearson")
corcov <- rbind(corcov, list("Hours_sleep", "MHCSF_total",MHCSF_total_Hours_sleep_cor ,MHCSF_total_Hours_sleep_cov ))

#Between Hours of Sleep and Positive Emotion
PE_avg_Hours_sleep_cor <- cor(dataset$PE_avg, dataset$Hours_sleep, use="complete.obs", method="pearson")
PE_avg_Hours_sleep_cov <- cov(dataset$PE_avg, dataset$Hours_sleep, use="complete.obs", method="pearson")
corcov <- rbind(corcov, list("Hours_sleep", "PE_avg",PE_avg_Hours_sleep_cor , PE_avg_Hours_sleep_cov))

#Between Hours of Sleep and resilience 
Resilience_avg_Hours_sleep_cor <- cor(dataset$Resilience_avg, dataset$Hours_sleep, use="complete.obs", method="pearson")
Resilience_avg_Hours_sleep_cov <- cov(dataset$Resilience_avg, dataset$Hours_sleep, use="complete.obs", method="pearson")
corcov <- rbind(corcov, list("Hours_sleep", "Resilience_avg",Resilience_avg_Hours_sleep_cor , Resilience_avg_Hours_sleep_cov))

#Between Stress and Depression
Depression_total_Overall_stress_cor <- cor(dataset$Overall_stress, dataset$Depression_total, use="complete.obs", method="pearson")
Depression_total_Overall_stress_cov <- cov(dataset$Overall_stress, dataset$Depression_total, use="complete.obs", method="pearson")
corcov <- rbind(corcov, list("Overall_stress", "Depression_total", Depression_total_Overall_stress_cor, Depression_total_Overall_stress_cov))

#Between Stress and Anxiety
Anxiety_total_Overall_stress_cor <- cor(dataset$Overall_stress, dataset$Anxiety_total, use="complete.obs", method="pearson")
Anxiety_total_Overall_stress_cov <- cov(dataset$Overall_stress, dataset$Anxiety_total, use="complete.obs", method="pearson")
corcov <- rbind(corcov, list("Overall_stress", "Anxiety_total", Anxiety_total_Overall_stress_cor, Anxiety_total_Overall_stress_cov))

#Between Stress and Mental Health Continuum Score
MHCSF_total_Overall_stress_cor <- cor(dataset$Overall_stress, dataset$MHCSF_total, use="complete.obs", method="pearson")
MHCSF_total_Overall_stress_cov <- cov(dataset$Overall_stress, dataset$MHCSF_total, use="complete.obs", method="pearson")
corcov <- rbind(corcov, list("Overall_stress", "MHCSF_total", MHCSF_total_Overall_stress_cor, MHCSF_total_Overall_stress_cov))

#Between Stress and Positive Emotion
PE_avg_Overall_stress_cor <- cor(dataset$Overall_stress, dataset$PE_avg, use="complete.obs", method="pearson")
PE_avg_Overall_stress_cov <- cov(dataset$Overall_stress, dataset$PE_avg, use="complete.obs", method="pearson")
corcov <- rbind(corcov, list("Overall_stress", "PE_avg", PE_avg_Overall_stress_cor, PE_avg_Overall_stress_cov))

#Between Stress and Resilience
Resilience_avg_Overall_stress_cor <- cor(dataset$Overall_stress, dataset$Resilience_avg, use="complete.obs", method="pearson")
Resilience_avg_Overall_stress_cov <- cov(dataset$Overall_stress, dataset$Resilience_avg, use="complete.obs", method="pearson")
corcov <- rbind(corcov, list("Overall_stress", "Resilience_avg", Resilience_avg_Overall_stress_cor,Resilience_avg_Overall_stress_cov ))

#Between Screen Time and Depression
Depression_total_Hours_screentime_cor <- cor(dataset$Hours_screentime, dataset$Depression_total, use="complete.obs", method="pearson")
Depression_total_Hours_screentime_cov <- cov(dataset$Hours_screentime, dataset$Depression_total, use="complete.obs", method="pearson")
corcov <- rbind(corcov, list("Hours_screentime", "Depression_total", Depression_total_Hours_screentime_cor,Depression_total_Hours_screentime_cov))

#Between Screen Time and Anxiety
Anxiety_total_Hours_screentime_cor <- cor(dataset$Hours_screentime, dataset$Anxiety_total, use="complete.obs", method="pearson")
Anxiety_total_Hours_screentime_cov <- cov(dataset$Hours_screentime, dataset$Anxiety_total, use="complete.obs", method="pearson")
corcov <- rbind(corcov, list("Hours_screentime", "Anxiety_total", Anxiety_total_Hours_screentime_cor, Anxiety_total_Hours_screentime_cov))

#Between Screen Time and Mental Health Continuum Score
MHCSF_total_Hours_screentime_cor <- cor(dataset$Hours_screentime, dataset$MHCSF_total, use="complete.obs", method="pearson")
MHCSF_total_Hours_screentime_cov <- cov(dataset$Hours_screentime, dataset$MHCSF_total, use="complete.obs", method="pearson")
corcov <- rbind(corcov, list("Hours_screentime", "MHCSF_total", MHCSF_total_Hours_screentime_cor, MHCSF_total_Hours_screentime_cov))

#Between Screen Time and Positive Emotion
PE_avg_Hours_screentime_cor <- cor(dataset$Hours_screentime, dataset$PE_avg, use="complete.obs", method="pearson")
PE_avg_Hours_screentime_cov <- cov(dataset$Hours_screentime, dataset$PE_avg, use="complete.obs", method="pearson")
corcov <- rbind(corcov, list("Hours_screentime", "PE_avg", PE_avg_Hours_screentime_cor, PE_avg_Hours_screentime_cov))

#Between Screen Time and Resilience
Resilience_avg_Hours_screentime_cor <- cor(dataset$Hours_screentime, dataset$Resilience_avg, use="complete.obs", method="pearson")
Resilience_avg_Hours_screentime_cov <- cov(dataset$Hours_screentime, dataset$Resilience_avg, use="complete.obs", method="pearson")
corcov <- rbind(corcov, list("Hours_screentime", "Resilience_avg", Resilience_avg_Hours_screentime_cor, Resilience_avg_Hours_screentime_cov))

#Between Screen Time and Overall Stress
Overall_stress_Hours_screentime_cor <- cor(dataset$Hours_screentime, dataset$Overall_stress, use="complete.obs", method="pearson")
Overall_stress_Hours_screentime_cov <- cov(dataset$Hours_screentime, dataset$Overall_stress, use="complete.obs", method="pearson")
corcov <- rbind(corcov, list("Hours_screentime", "Overall_stress", Overall_stress_Hours_screentime_cor,Overall_stress_Hours_screentime_cov ))


#################################################################################
# 
#################################################################################

#plot(dataset$Overall_MH, dataset$Overall_stress,xlab="Overall Mental Health",
#     ylab="Overall Stress", main="Stress vs Mental Health", frame.plot=FALSE, 
#     col=ifelse(dataset$Gender==1,"red",ifelse(dataset$Gender==2, "blue", "yellow")))
#legend("topleft", pch=c(2,2,2), col=c("red", "blue", "yellow"), 
#       c("Male", "Female", "Other"), bty="o", cex=.8)

correlations <- cor(dataset[sapply(dataset, is.numeric)], use="pairwise", method="pearson")

#Be aware that the headers will be shifted over one if you open in Excel
#Export the dataframes to a CSV:
write.table(analysis, "C:/Users/kates/Desktop/HWBI/HWBS/HWBS2017_Analysis.txt", sep=",")
write.table(corcov, "C:/Users/kates/Desktop/HWBI/HWBS/HWBS2017_CorCov.txt", sep=",")
write.table(correlations, "C:/Users/kates/Desktop/HWBI/HWBS/HWBS2017_Correlations.txt", sep=",")