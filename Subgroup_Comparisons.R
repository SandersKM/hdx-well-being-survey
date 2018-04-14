#For Comparing Means between Subgroups of the dataset

# You will need to instal these libraries is they aren't in your own RStudio. 
# Use: install.packages("packagename")

# Libraries for graphing in R
library(rlang)
library(ggplot2)
library(data.table) # for the set method
library(readxl) #library used to import Excel data
# import the Hendrix Well Being Survey Data (change location for your own HWBS Data)
dataset <- read_excel("C:/Users/kates/Desktop/HWBS_STUDENTS_2017_condensed.xlsx")

# Used to calculate Standard Deviation  
std <- function(col){
  var <- var(col, na.rm = TRUE)
  return(sqrt(var))
}



# Initializes a dataframe with correct column names
t_tests <- data.frame("Factor"="","x"="", "y"='',
                      "mean of x"=0.0,"std of x"=0.0,
                      "mean of y"= 0.0,"std of y"=0.0,
                      "p.value"=0.0, stringsAsFactors = FALSE)

# Extracts the important information from a t-test
t_test <- function(x) {
  c(x$estimate[1],
    x$estimate[2],
    ci.lower = x$conf.int[1],
    ci.upper = x$conf.int[2],
    p.value = x$p.value)
}

# Splits data between one subgroup an  all other subroups
to_table_excluding <- function(x,xName, yName, sortOn,c, cName) {
  t <- t_test(t.test(c[sortOn==x], c[sortOn!=x]))
  return (list(cName,xName,  yName,
               t[[1]], std(c[sortOn==x]), 
               t[[2]],std(c[sortOn!=x]), t[[5]]))
}

# compares the data of everything but one subgroup to total data
to_table_general_excluding <- function(x,xName,sortOn,c, cName) {
  t <- t_test(t.test(c[sortOn!=x], c))
  return (list(cName,"All_Data",xName,  
               t[[1]], std(c[sortOn!=x]), 
               t[[2]],std(c), t[[5]]))
}

# compares the data of 2 subgroups
to_table <- function(x,y,xName, yName, sortOn,c, cName) {
  t <- t_test(t.test(c[sortOn==x], c[sortOn==y]))
  return (list(cName,xName,  yName,
                 t[[1]], std(c[sortOn==x]), 
                 t[[2]],std(c[sortOn==y]),t[[5]]))
}

# compares the data of one subgroup to the full group
to_table_general <- function(x,xName,sortOn,c, cName) {
  t <- t_test(t.test(c[sortOn==x], c))
  return (list(cName,"All_Data",xName,  
               t[[1]], std(c[sortOn==x]), 
               t[[2]],std(c), t[[5]]))
}

##########################################################################################################################
# Compare By Gender
##########################################################################################################################

gender_compare <- function(dataframe, c, cName){
  dataframe <- rbind(dataframe, to_table_general(1, "Male", dataset$Gender, c, cName))
  dataframe <- rbind(dataframe, to_table_general(2, "Female", dataset$Gender, c, cName))
  dataframe <- rbind(dataframe, to_table_general(3, "Other", dataset$Gender, c, cName))
  dataframe <- rbind(dataframe, to_table(1,2,"Male", "Female",dataset$Gender,c, cName))
  dataframe <- rbind(dataframe, to_table(1,3,"Male", "Other",dataset$Gender,c, cName))
  dataframe <- rbind(dataframe, to_table(3,2,"Other", "Female",dataset$Gender,c, cName))
  return(dataframe)
}

gender_compile <- function(dataframe){
  dataframe <- gender_compare(dataframe, dataset$Overall_MH, "Overall_MH")
  dataframe <- gender_compare(dataframe, dataset$Overall_stress, "Overall_stress")
  dataframe <- gender_compare(dataframe, dataset$Depression_total, "Depression_total")
  dataframe <- gender_compare(dataframe, dataset$Anxiety_academic_impact, "Anxiety_total")
  dataframe <- gender_compare(dataframe, dataset$MHCSF_total, "MHCSF_total")
  dataframe <- gender_compare(dataframe, dataset$HDX_MH_impact, "HDX_MH_impact")
  dataframe <- gender_compare(dataframe, dataset$MI_identity, "MI_identity")
  dataframe <- gender_compare(dataframe, dataset$MH_needs, "MH_needs")
  dataframe <- gender_compare(dataframe, dataset$MH_needs_met, "MH_needs_met")
  dataframe <- gender_compare(dataframe, dataset$MH_training, "MH_training")
  dataframe <- gender_compare(dataframe, dataset$MH_academic_impact, "MH_academic_impact")
  dataframe <- gender_compare(dataframe, dataset$Belonging_total, "Belonging_total")
  return(dataframe[-1,])
}

##########################################################################################################################
# Compare Trans/Cis 
##########################################################################################################################

trans_compare <- function(dataframe, c, cName){
  dataframe <- rbind(dataframe, to_table_general(1, "Trans", dataset$Trans, c, cName))
  dataframe <- rbind(dataframe, to_table_general(2, "Cis", dataset$Trans, c, cName))
  dataframe <- rbind(dataframe, to_table(1,2,"Trans", "Cis",dataset$Trans,c, cName))
  return(dataframe)
}

trans_compile <- function(dataframe){
  dataframe <- trans_compare(dataframe, dataset$Overall_MH, "Overall_MH")
  dataframe <- trans_compare(dataframe, dataset$Overall_stress, "Overall_stress")
  dataframe <- trans_compare(dataframe, dataset$Depression_total, "Depression_total")
  dataframe <- trans_compare(dataframe, dataset$Anxiety_academic_impact, "Anxiety_total")
  dataframe <- trans_compare(dataframe, dataset$MHCSF_total, "MHCSF_total")
  dataframe <- trans_compare(dataframe, dataset$HDX_MH_impact, "HDX_MH_impact")
  dataframe <- trans_compare(dataframe, dataset$MI_identity, "MI_identity")
  dataframe <- trans_compare(dataframe, dataset$MH_needs, "MH_needs")
  dataframe <- trans_compare(dataframe, dataset$MH_needs_met, "MH_needs_met")
  dataframe <- trans_compare(dataframe, dataset$MH_training, "MH_training")
  dataframe <- trans_compare(dataframe, dataset$MH_academic_impact, "MH_academic_impact")
  dataframe <- trans_compare(dataframe, dataset$Belonging_total, "Belonging_total")
  return(dataframe[-1,])
}

##########################################################################################################################
# Compare By Race
##########################################################################################################################

race1_compare <- function(dataframe, c, cName){
  dataframe <- rbind(dataframe, to_table_general(7, "White", dataset$Race, c, cName))
  dataframe <- rbind(dataframe, to_table_general_excluding(7, "POC", dataset$Race, c, cName))
  dataframe <- rbind(dataframe, to_table_excluding(7,"White", "POC",dataset$Race,c, cName))
  return(dataframe)
}

race1_compile <- function(dataframe){
  dataframe <- race1_compare(dataframe, dataset$Overall_MH, "Overall_MH")
  dataframe <- race1_compare(dataframe, dataset$Overall_stress, "Overall_stress")
  dataframe <- race1_compare(dataframe, dataset$Depression_total, "Depression_total")
  dataframe <- race1_compare(dataframe, dataset$Anxiety_academic_impact, "Anxiety_total")
  dataframe <- race1_compare(dataframe, dataset$MHCSF_total, "MHCSF_total")
  dataframe <- race1_compare(dataframe, dataset$HDX_MH_impact, "HDX_MH_impact")
  dataframe <- race1_compare(dataframe, dataset$MI_identity, "MI_identity")
  dataframe <- race1_compare(dataframe, dataset$MH_needs, "MH_needs")
  dataframe <- race1_compare(dataframe, dataset$MH_needs_met, "MH_needs_met")
  dataframe <- race1_compare(dataframe, dataset$MH_training, "MH_training")
  dataframe <- race1_compare(dataframe, dataset$MH_academic_impact, "MH_academic_impact")
  dataframe <- race1_compare(dataframe, dataset$Belonging_total, "Belonging_total")
  return(dataframe[-1,])
}

##########################################################################################################################
#
##########################################################################################################################
