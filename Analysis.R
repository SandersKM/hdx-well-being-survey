# Main analysis file for the Hendrix Well Being Initiative Survey Analysis
# Created by Kate Sanders in Spring 2018
# After running this file, you can get the desired values by typing:
# Mean: categoryname_mean
# Variance: categoryname_var
# Standard Deviation: categoryname_std
# Response Rate: categoryname_rr
# Some of the categories other anayses accessed as: categoryname_anaysisname
# All of these values will appear automatically if you just run the subanalysis files


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
Overall_MH_R_mean <- mean(dataset$Overall_MH_R, na.rm=TRUE)
Overall_MH_R_var <- var(dataset$Overall_MH_R, na.rm = TRUE)
Overall_MH_R_std <- sqrt(Overall_MH_R_var)
Overall_MH_R_rr <- response_rate(dataset$Overall_MH_R)

#“I see myself as a person with mental illness” (scale:1-6)
MI_identity_R_mean <- mean(dataset$MI_identity_R, na.rm=TRUE) 
MI_identity_R_var <- var(dataset$MI_identity_R, na.rm = TRUE)
MI_identity_R_std <- sqrt(MI_identity_R_var)
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

#hours per week exercising (open-ended)
Hours_exercising_min <- min(dataset$Hours_exercising, na.rm=TRUE) #minimum
Hours_exercising_max <- max(dataset$Hours_exercising, na.rm=TRUE) #maximum
Hours_exercising_mean <- mean(dataset$Hours_exercising, na.rm=TRUE) #including outliers
Hours_exercising_var <- var(dataset$Hours_exercising, na.rm = TRUE)
Hours_exercising_std <- sqrt(Hours_exercising_var)
Hours_exercising_box <- boxplot(dataset$Hours_exercising) #Boxplot with outliers
Hours_exercising_rr <- response_rate(dataset$Hours_exercising)
Hours_exercising_q1 <- summary(dataset$Hours_exercising)[["1st Qu."]]
Hours_exercising_q3  <-summary(dataset$Hours_exercising)[["3rd Qu."]]
Hours_exercising_iqr <- Q3_hours_exercising - Q1_hours_exercising #IQR

# typical hours of sleep per night (open-ended)
Hours_sleep_min <- min(dataset$Hours_sleep, na.rm=TRUE) #minimum
Hours_sleep_max <- max(dataset$Hours_sleep, na.rm=TRUE) #maximum
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
Depression_interference_var <- var(datset$Depression_interference, na.rm=TRUE)
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

#################################################################################
# 
#################################################################################
