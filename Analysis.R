
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

#hours per week exercising (open-ended)
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
# Positive mental health
#################################################################################

# Mental Health Continuum – Short Form (scale:0-70)
mean(dataset$MHCSF_total, na.rm=TRUE) 
response_rate(dataset$MHCSF_total)
boxplot(dataset$MHCSF_total)

#Level of positive emotions (scale 0-4)
mean(dataset$PE_avg, na.rm=TRUE) 
response_rate(dataset$PE_avg)
boxplot(dataset$PE_avg)

# Level of resilience (scale 1-5)
mean(dataset$Resilience_avg, na.rm=TRUE) 
response_rate(dataset$Resilience_avg)
boxplot(dataset$Resilience_avg)

# Level of satisfaction with personal 
#relationships at HDX (scale 1-5)
mean(dataset$Relationship_satis, na.rm=TRUE) 
response_rate(dataset$Relationship_satis)
boxplot(dataset$Relationship_satis)

# Level of belongingness (scale 9-54)
mean(dataset$Belonging_total, na.rm=TRUE) 
response_rate(dataset$Belonging_total)
boxplot(dataset$Belonging_total)

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
flourishing_count/rownum

#################################################################################
# Depression and anxiety
#################################################################################

# Level of depression.
# 0-4 = no depression
# 5-9 = minimal depression
# 10-14 = mild depression
# 15-19 = major depression
# > 20 = severe major depression
mean(dataset$Depression_total, na.rm=TRUE) 
response_rate(dataset$Depression_total)
boxplot(dataset$Depression_total)

# Level of impairment from depression (scale: 0-6)
mean(dataset$Depression_interference, na.rm=TRUE) 
response_rate(dataset$Depression_interference)
boxplot(dataset$Depression_interference)

# Level of anxiety 
# 5-9 = mild anxiety; 
# 10-14 = moderate anxiety; 
# 15-21 = severe anxiety
mean(dataset$Anxiety_total, na.rm=TRUE) 
response_rate(dataset$Anxiety_total)
boxplot(dataset$Anxiety_total)

# Level of impairment from anxiety (scale: 0-6)
mean(dataset$Anxiety_interference, na.rm=TRUE) 
response_rate(dataset$Anxiety_interference)
boxplot(dataset$Anxiety_interference)

#################################################################################
# Non-suicidal self-injury and eating behavior
#################################################################################

# Frequency of non-suicidal self-injury as percentage of sample
# who engaged in at least one type of NSSI over the past year 
#?????

# Level of eating behavior impairment 
# (scale:0-45) >16 = At Risk
mean(dataset$ED_total, na.rm=TRUE) 
response_rate(dataset$ED_total)

#################################################################################
# 
#################################################################################
