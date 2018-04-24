#For Comparing Means between Subgroups of the dataset

# You will need to instal these libraries is they aren't in your own RStudio. 
# Use: install.packages("packagename")

# Libraries for graphing in R
library(rlang)
library(ggplot2)
library(data.table) # for the set method
library(readxl) #library used to import Excel data
# import the Hendrix Well Being Survey Data (change location for your own HWBS Data)
dataset <- read_excel("C:/Users/kates/Desktop/HWBI/HWBS/HWBS_STUDENTS_2017_condensed.xlsx")

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
  return (list(cName,xName,  "All_Data",
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
  return (list(cName,xName,  "All_Data",
               t[[1]], std(c[sortOn==x]), 
               t[[2]],std(c), t[[5]]))
}

# compares the data of 2 subgroups from different columns
to_table_different <- function(xName, yName, sortOn1, sortOn2, c, cName) {
  t <- t_test(t.test(c[sortOn1==1], c[sortOn2==1]))
  return (list(cName,xName,  yName,
               t[[1]], std(c[sortOn1==1]), 
               t[[2]],std(c[sortOn2==1]),t[[5]]))
}


##########################################################################################################################
# Compile Comparisons
##########################################################################################################################

write.table(gender_compile(t_tests), "C:/Users/kates/Desktop/HWBI/Subgroup_Comparisons/gender.txt", sep=",")
write.table(trans_compile(t_tests), "C:/Users/kates/Desktop/HWBI/Subgroup_Comparisons/trans.txt", sep=",")
write.table(race1_compile(t_tests), "C:/Users/kates/Desktop/HWBI/Subgroup_Comparisons/race1.txt", sep=",")
write.table(race2_compile(t_tests), "C:/Users/kates/Desktop/HWBI/Subgroup_Comparisons/race2.txt", sep=",")
write.table(sexuality1_compile(t_tests), "C:/Users/kates/Desktop/HWBI/Subgroup_Comparisons/sexuality1.txt", sep=",")
write.table(sexuality2_compile(t_tests), "C:/Users/kates/Desktop/HWBI/Subgroup_Comparisons/sexuality2.txt", sep=",")
write.table(SES_compile(t_tests), "C:/Users/kates/Desktop/HWBI/Subgroup_Comparisons/SES.txt", sep=",")
write.table(Religion_compile(t_tests), "C:/Users/kates/Desktop/HWBI/Subgroup_Comparisons/religion.txt", sep=",")
write.table(year_compile(t_tests), "C:/Users/kates/Desktop/HWBI/Subgroup_Comparisons/year.txt", sep=",")
write.table(transfer_compile(t_tests), "C:/Users/kates/Desktop/HWBI/Subgroup_Comparisons/transfer.txt", sep=",")
write.table(first_gen_compile(t_tests), "C:/Users/kates/Desktop/HWBI/Subgroup_Comparisons/first_gen.txt", sep=",")
write.table(International_compile(t_tests), "C:/Users/kates/Desktop/HWBI/Subgroup_Comparisons/international.txt", sep=",")
write.table(disability_compile(t_tests), "C:/Users/kates/Desktop/HWBI/Subgroup_Comparisons/disability.txt", sep=",")
write.table(Major_compile(t_tests), "C:/Users/kates/Desktop/HWBI/Subgroup_Comparisons/major.txt", sep=",")


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
  dataframe <- gender_compare(dataframe, dataset$Anxiety_total, "Anxiety_total")
  dataframe <- gender_compare(dataframe, dataset$MHCSF_total, "MHCSF_total")
  dataframe <- gender_compare(dataframe, dataset$HDX_MH_impact, "HDX_MH_impact")
  dataframe <- gender_compare(dataframe, dataset$MI_identity_R_R_R, "MI_identity_R_R_R")
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
  dataframe <- trans_compare(dataframe, dataset$Anxiety_total, "Anxiety_total")
  dataframe <- trans_compare(dataframe, dataset$MHCSF_total, "MHCSF_total")
  dataframe <- trans_compare(dataframe, dataset$HDX_MH_impact, "HDX_MH_impact")
  dataframe <- trans_compare(dataframe, dataset$MI_identity_R_R, "MI_identity_R_R")
  dataframe <- trans_compare(dataframe, dataset$MH_needs, "MH_needs")
  dataframe <- trans_compare(dataframe, dataset$MH_needs_met, "MH_needs_met")
  dataframe <- trans_compare(dataframe, dataset$MH_training, "MH_training")
  dataframe <- trans_compare(dataframe, dataset$MH_academic_impact, "MH_academic_impact")
  dataframe <- trans_compare(dataframe, dataset$Belonging_total, "Belonging_total")
  return(dataframe[-1,])
}

##########################################################################################################################
# Compare By Race (White vs POC)
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
  dataframe <- race1_compare(dataframe, dataset$Anxiety_total, "Anxiety_total")
  dataframe <- race1_compare(dataframe, dataset$MHCSF_total, "MHCSF_total")
  dataframe <- race1_compare(dataframe, dataset$HDX_MH_impact, "HDX_MH_impact")
  dataframe <- race1_compare(dataframe, dataset$MI_identity_R, "MI_identity_R")
  dataframe <- race1_compare(dataframe, dataset$MH_needs, "MH_needs")
  dataframe <- race1_compare(dataframe, dataset$MH_needs_met, "MH_needs_met")
  dataframe <- race1_compare(dataframe, dataset$MH_training, "MH_training")
  dataframe <- race1_compare(dataframe, dataset$MH_academic_impact, "MH_academic_impact")
  dataframe <- race1_compare(dataframe, dataset$Belonging_total, "Belonging_total")
  return(dataframe[-1,])
}

##########################################################################################################################
# Compare By Race (Less Generalized)
##########################################################################################################################

race2_compare <- function(dataframe, c, cName){
  dataframe <- rbind(dataframe, to_table_general(7, "White", dataset$Race, c, cName))
  dataframe <- rbind(dataframe, to_table_general(2, "Asian", dataset$Race, c, cName))
  dataframe <- rbind(dataframe, to_table_general(3, "Black", dataset$Race, c, cName))
  dataframe <- rbind(dataframe, to_table_general(4, "Hispanic", dataset$Race, c, cName))
  dataframe <- rbind(dataframe, to_table_general(5, "Middle_Eastern", dataset$Race, c, cName))
  dataframe <- rbind(dataframe, to_table_general(8, "Multiracial", dataset$Race, c, cName))
  dataframe <- rbind(dataframe, to_table(7, 2,"White", "Asian",dataset$Race,c, cName))
  dataframe <- rbind(dataframe, to_table(7, 3,"White", "Black",dataset$Race,c, cName))
  dataframe <- rbind(dataframe, to_table(7, 4,"White", "Hispanic",dataset$Race,c, cName))
  dataframe <- rbind(dataframe, to_table(7, 5,"White", "Middle_Eastern",dataset$Race,c, cName))
  dataframe <- rbind(dataframe, to_table(7, 8,"White", "Multiracial",dataset$Race,c, cName))
  return(dataframe)
}

race2_compile <- function(dataframe){
  dataframe <- race2_compare(dataframe, dataset$Overall_MH, "Overall_MH")
  dataframe <- race2_compare(dataframe, dataset$Overall_stress, "Overall_stress")
  dataframe <- race2_compare(dataframe, dataset$Depression_total, "Depression_total")
  dataframe <- race2_compare(dataframe, dataset$Anxiety_total, "Anxiety_total")
  dataframe <- race2_compare(dataframe, dataset$MHCSF_total, "MHCSF_total")
  dataframe <- race2_compare(dataframe, dataset$HDX_MH_impact, "HDX_MH_impact")
  dataframe <- race2_compare(dataframe, dataset$MI_identity_R, "MI_identity_R")
  dataframe <- race2_compare(dataframe, dataset$MH_needs, "MH_needs")
  dataframe <- race2_compare(dataframe, dataset$MH_needs_met, "MH_needs_met")
  dataframe <- race2_compare(dataframe, dataset$MH_training, "MH_training")
  dataframe <- race2_compare(dataframe, dataset$MH_academic_impact, "MH_academic_impact")
  dataframe <- race2_compare(dataframe, dataset$Belonging_total, "Belonging_total")
  return(dataframe[-1,])
}

##########################################################################################################################
# Compare By Sexuality
##########################################################################################################################

sexuality1_compare <- function(dataframe, c, cName){
  dataframe <- rbind(dataframe, to_table_general(9, "Heterosexual", dataset$Sexual_orientation, c, cName))
  dataframe <- rbind(dataframe, to_table_general_excluding(9, "LGBTQ+", dataset$Sexual_orientation, c, cName))
  dataframe <- rbind(dataframe, to_table_excluding(9,"Heterosexual", "LGBTQ+",dataset$Sexual_orientation,c, cName))
  return(dataframe)
}

sexuality1_compile <- function(dataframe){
  dataframe <- sexuality1_compare(dataframe, dataset$Overall_MH, "Overall_MH")
  dataframe <- sexuality1_compare(dataframe, dataset$Overall_stress, "Overall_stress")
  dataframe <- sexuality1_compare(dataframe, dataset$Depression_total, "Depression_total")
  dataframe <- sexuality1_compare(dataframe, dataset$Anxiety_total, "Anxiety_total")
  dataframe <- sexuality1_compare(dataframe, dataset$MHCSF_total, "MHCSF_total")
  dataframe <- sexuality1_compare(dataframe, dataset$HDX_MH_impact, "HDX_MH_impact")
  dataframe <- sexuality1_compare(dataframe, dataset$MI_identity_R, "MI_identity_R")
  dataframe <- sexuality1_compare(dataframe, dataset$MH_needs, "MH_needs")
  dataframe <- sexuality1_compare(dataframe, dataset$MH_needs_met, "MH_needs_met")
  dataframe <- sexuality1_compare(dataframe, dataset$MH_training, "MH_training")
  dataframe <- sexuality1_compare(dataframe, dataset$MH_academic_impact, "MH_academic_impact")
  dataframe <- sexuality1_compare(dataframe, dataset$Belonging_total, "Belonging_total")
  return(dataframe[-1,])
}

##########################################################################################################################
# Compare By Sexuality (Less Generalized)
##########################################################################################################################

sexuality2compare <- function(dataframe, c, cName){
  dataframe <- rbind(dataframe, to_table_general(0, "Other", dataset$Sexual_orientation, c, cName))
  dataframe <- rbind(dataframe, to_table_general(1, "Asexual", dataset$Sexual_orientation, c, cName))
  dataframe <- rbind(dataframe, to_table_general(2, "Bisexual", dataset$Sexual_orientation, c, cName))
  dataframe <- rbind(dataframe, to_table_general(4, "Lesbian", dataset$Sexual_orientation, c, cName))
  dataframe <- rbind(dataframe, to_table_general(5, "Pansexual", dataset$Sexual_orientation, c, cName))
  dataframe <- rbind(dataframe, to_table_general(6, "Queer", dataset$Sexual_orientation, c, cName))
  dataframe <- rbind(dataframe, to_table_general(7, "Questioning", dataset$Sexual_orientation, c, cName))
  dataframe <- rbind(dataframe, to_table_general(3, "Gay", dataset$Sexual_orientation, c, cName))
  dataframe <- rbind(dataframe, to_table_general(9, "Heterosexual", dataset$Sexual_orientation, c, cName))
  dataframe <- rbind(dataframe, to_table(9,0,"Heterosexual", "Other",dataset$Sexual_orientation,c, cName))
  dataframe <- rbind(dataframe, to_table(9,1,"Heterosexual", "Asexual",dataset$Sexual_orientation,c, cName))
  dataframe <- rbind(dataframe, to_table(9,2,"Heterosexual", "Bisexual",dataset$Sexual_orientation,c, cName))
  dataframe <- rbind(dataframe, to_table(9,3,"Heterosexual", "Gay",dataset$Sexual_orientation,c, cName))
  dataframe <- rbind(dataframe, to_table(9,4,"Heterosexual", "Lesbian",dataset$Sexual_orientation,c, cName))
  dataframe <- rbind(dataframe, to_table(9,5,"Heterosexual", "Pansexual",dataset$Sexual_orientation,c, cName))
  dataframe <- rbind(dataframe, to_table(9,6,"Heterosexual", "Queer",dataset$Sexual_orientation,c, cName))
  dataframe <- rbind(dataframe, to_table(9,7,"Heterosexual", "Questioning",dataset$Sexual_orientation,c, cName))
  return(dataframe)
}

sexuality2_compile <- function(dataframe){
  dataframe <- sexuality2compare(dataframe, dataset$Overall_MH, "Overall_MH")
  dataframe <- sexuality2compare(dataframe, dataset$Overall_stress, "Overall_stress")
  dataframe <- sexuality2compare(dataframe, dataset$Depression_total, "Depression_total")
  dataframe <- sexuality2compare(dataframe, dataset$Anxiety_total, "Anxiety_total")
  dataframe <- sexuality2compare(dataframe, dataset$MHCSF_total, "MHCSF_total")
  dataframe <- sexuality2compare(dataframe, dataset$HDX_MH_impact, "HDX_MH_impact")
  dataframe <- sexuality2compare(dataframe, dataset$MI_identity_R, "MI_identity_R")
  dataframe <- sexuality2compare(dataframe, dataset$MH_needs, "MH_needs")
  dataframe <- sexuality2compare(dataframe, dataset$MH_needs_met, "MH_needs_met")
  dataframe <- sexuality2compare(dataframe, dataset$MH_training, "MH_training")
  dataframe <- sexuality2compare(dataframe, dataset$MH_academic_impact, "MH_academic_impact")
  dataframe <- sexuality2compare(dataframe, dataset$Belonging_total, "Belonging_total")
  return(dataframe[-1,])
}

##########################################################################################################################
# Compare By SESrung
##########################################################################################################################

SES_compare <- function(dataframe, c, cName){
  dataframe <- rbind(dataframe, to_table_general(1, "1", dataset$SESrung, c, cName))
  dataframe <- rbind(dataframe, to_table_general(2, "2", dataset$SESrung, c, cName))
  dataframe <- rbind(dataframe, to_table_general(3, "3", dataset$SESrung, c, cName))
  dataframe <- rbind(dataframe, to_table_general(4, "4", dataset$SESrung, c, cName))
  dataframe <- rbind(dataframe, to_table_general(5, "5", dataset$SESrung, c, cName))
  dataframe <- rbind(dataframe, to_table_general(6, "6", dataset$SESrung, c, cName))
  dataframe <- rbind(dataframe, to_table_general(7, "7", dataset$SESrung, c, cName))
  dataframe <- rbind(dataframe, to_table_general(8, "8", dataset$SESrung, c, cName))
  dataframe <- rbind(dataframe, to_table_general(9, "9", dataset$SESrung, c, cName))
  dataframe <- rbind(dataframe, to_table_general(10, "10", dataset$SESrung, c, cName))
  dataframe <- rbind(dataframe, to_table(1, 10, "1", "10", dataset$SESrung, c, cName))
  dataframe <- rbind(dataframe, to_table(1, 5, "1", "5", dataset$SESrung, c, cName))
  dataframe <- rbind(dataframe, to_table(5, 10, "5", "10", dataset$SESrung, c, cName))
  return(dataframe)
}

SES_compile <- function(dataframe){
  dataframe <- SES_compare(dataframe, dataset$Overall_MH, "Overall_MH")
  dataframe <- SES_compare(dataframe, dataset$Overall_stress, "Overall_stress")
  dataframe <- SES_compare(dataframe, dataset$Depression_total, "Depression_total")
  dataframe <- SES_compare(dataframe, dataset$Anxiety_total, "Anxiety_total")
  dataframe <- SES_compare(dataframe, dataset$MHCSF_total, "MHCSF_total")
  dataframe <- SES_compare(dataframe, dataset$HDX_MH_impact, "HDX_MH_impact")
  dataframe <- SES_compare(dataframe, dataset$MI_identity_R, "MI_identity_R")
  dataframe <- SES_compare(dataframe, dataset$MH_needs, "MH_needs")
  dataframe <- SES_compare(dataframe, dataset$MH_needs_met, "MH_needs_met")
  dataframe <- SES_compare(dataframe, dataset$MH_training, "MH_training")
  dataframe <- SES_compare(dataframe, dataset$MH_academic_impact, "MH_academic_impact")
  dataframe <- SES_compare(dataframe, dataset$Belonging_total, "Belonging_total")
  return(dataframe[-1,])
}

##########################################################################################################################
# Compare By Religious Affiliation
##########################################################################################################################

Religion_compare <- function(dataframe, c, cName){
  dataframe <- rbind(dataframe, to_table_general(1, "Agnostic", dataset$Religious_affiliation, c, cName))
  dataframe <- rbind(dataframe, to_table_general(2, "Athiest", dataset$Religious_affiliation, c, cName))
  dataframe <- rbind(dataframe, to_table_general_excluding(2, "Not_Athiest", dataset$Religious_affiliation, c, cName))
  dataframe <- rbind(dataframe, to_table_general(4, "Catholic", dataset$Religious_affiliation, c, cName))
  dataframe <- rbind(dataframe, to_table_general(5, "Christian", dataset$Religious_affiliation, c, cName))
  dataframe <- rbind(dataframe, to_table_general(7, "Jewish", dataset$Religious_affiliation, c, cName))
  dataframe <- rbind(dataframe, to_table_general(9, "Muslim", dataset$Religious_affiliation, c, cName))
  dataframe <- rbind(dataframe, to_table_general(10, "No_Pref", dataset$Religious_affiliation, c, cName))
  dataframe <- rbind(dataframe, to_table_general(11, "Multiple", dataset$Religious_affiliation, c, cName))
  dataframe <- rbind(dataframe, to_table_general(12, "Other", dataset$Religious_affiliation, c, cName))
  return(dataframe)
}

Religion_compile <- function(dataframe){
  dataframe <- Religion_compare(dataframe, dataset$Overall_MH, "Overall_MH")
  dataframe <- Religion_compare(dataframe, dataset$Overall_stress, "Overall_stress")
  dataframe <- Religion_compare(dataframe, dataset$Depression_total, "Depression_total")
  dataframe <- Religion_compare(dataframe, dataset$Anxiety_total, "Anxiety_total")
  dataframe <- Religion_compare(dataframe, dataset$MHCSF_total, "MHCSF_total")
  dataframe <- Religion_compare(dataframe, dataset$HDX_MH_impact, "HDX_MH_impact")
  dataframe <- Religion_compare(dataframe, dataset$MI_identity_R, "MI_identity_R")
  dataframe <- Religion_compare(dataframe, dataset$MH_needs, "MH_needs")
  dataframe <- Religion_compare(dataframe, dataset$MH_needs_met, "MH_needs_met")
  dataframe <- Religion_compare(dataframe, dataset$MH_training, "MH_training")
  dataframe <- Religion_compare(dataframe, dataset$MH_academic_impact, "MH_academic_impact")
  dataframe <- Religion_compare(dataframe, dataset$Belonging_total, "Belonging_total")
  return(dataframe[-1,])
}

##########################################################################################################################
# Compare By Year
##########################################################################################################################

year_compare <- function(dataframe, c, cName){
  dataframe <- rbind(dataframe, to_table_general(1, "Freshman", dataset$Year, c, cName))
  dataframe <- rbind(dataframe, to_table_general(2, "Sophomore", dataset$Year, c, cName))
  dataframe <- rbind(dataframe, to_table_general(3, "Junior", dataset$Year, c, cName))
  dataframe <- rbind(dataframe, to_table_general(4, "Senior", dataset$Year, c, cName))
  dataframe <- rbind(dataframe, to_table_general(5, "Super_Senior", dataset$Year, c, cName))
  dataframe <- rbind(dataframe, to_table_general(6, "Graduate", dataset$Year, c, cName))
  dataframe <- rbind(dataframe, to_table(1, 3, "Freshman", "Junior", dataset$Year, c, cName))
  dataframe <- rbind(dataframe, to_table(1, 4, "Freshman", "Senior", dataset$Year, c, cName))
  return(dataframe)
}

year_compile <- function(dataframe){
  dataframe <- year_compare(dataframe, dataset$Overall_MH, "Overall_MH")
  dataframe <- year_compare(dataframe, dataset$Overall_stress, "Overall_stress")
  dataframe <- year_compare(dataframe, dataset$Depression_total, "Depression_total")
  dataframe <- year_compare(dataframe, dataset$Anxiety_total, "Anxiety_total")
  dataframe <- year_compare(dataframe, dataset$MHCSF_total, "MHCSF_total")
  dataframe <- year_compare(dataframe, dataset$HDX_MH_impact, "HDX_MH_impact")
  dataframe <- year_compare(dataframe, dataset$MI_identity_R, "MI_identity_R")
  dataframe <- year_compare(dataframe, dataset$MH_needs, "MH_needs")
  dataframe <- year_compare(dataframe, dataset$MH_needs_met, "MH_needs_met")
  dataframe <- year_compare(dataframe, dataset$MH_training, "MH_training")
  dataframe <- year_compare(dataframe, dataset$MH_academic_impact, "MH_academic_impact")
  dataframe <- year_compare(dataframe, dataset$Belonging_total, "Belonging_total")
  return(dataframe[-1,])
}

##########################################################################################################################
# Compare Transfer
##########################################################################################################################

transfer_compare <- function(dataframe, c, cName){
  dataframe <- rbind(dataframe, to_table_general(1, "Transfer", dataset$Transfer_student, c, cName))
  dataframe <- rbind(dataframe, to_table_general(2, "Not_Transfer", dataset$Transfer_student, c, cName))
  dataframe <- rbind(dataframe, to_table(1,2,"Transfer", "Not_Transfer",dataset$Transfer_student,c, cName))
  return(dataframe)
}

transfer_compile <- function(dataframe){
  dataframe <- transfer_compare(dataframe, dataset$Overall_MH, "Overall_MH")
  dataframe <- transfer_compare(dataframe, dataset$Overall_stress, "Overall_stress")
  dataframe <- transfer_compare(dataframe, dataset$Depression_total, "Depression_total")
  dataframe <- transfer_compare(dataframe, dataset$Anxiety_total, "Anxiety_total")
  dataframe <- transfer_compare(dataframe, dataset$MHCSF_total, "MHCSF_total")
  dataframe <- transfer_compare(dataframe, dataset$HDX_MH_impact, "HDX_MH_impact")
  dataframe <- transfer_compare(dataframe, dataset$MI_identity_R, "MI_identity_R")
  dataframe <- transfer_compare(dataframe, dataset$MH_needs, "MH_needs")
  dataframe <- transfer_compare(dataframe, dataset$MH_needs_met, "MH_needs_met")
  dataframe <- transfer_compare(dataframe, dataset$MH_training, "MH_training")
  dataframe <- transfer_compare(dataframe, dataset$MH_academic_impact, "MH_academic_impact")
  dataframe <- transfer_compare(dataframe, dataset$Belonging_total, "Belonging_total")
  return(dataframe[-1,])
}

##########################################################################################################################
# Compare International
##########################################################################################################################

International_compare <- function(dataframe, c, cName){
  dataframe <- rbind(dataframe, to_table_general(1, "International", dataset$International_status, c, cName))
  dataframe <- rbind(dataframe, to_table_general(2, "Not_International", dataset$International_status, c, cName))
  dataframe <- rbind(dataframe, to_table(1,2,"International", "Not_International",dataset$International_status,c, cName))
  return(dataframe)
}

International_compile <- function(dataframe){
  dataframe <- International_compare(dataframe, dataset$Overall_MH, "Overall_MH")
  dataframe <- International_compare(dataframe, dataset$Overall_stress, "Overall_stress")
  dataframe <- International_compare(dataframe, dataset$Depression_total, "Depression_total")
  dataframe <- International_compare(dataframe, dataset$Anxiety_total, "Anxiety_total")
  dataframe <- International_compare(dataframe, dataset$MHCSF_total, "MHCSF_total")
  dataframe <- International_compare(dataframe, dataset$HDX_MH_impact, "HDX_MH_impact")
  dataframe <- International_compare(dataframe, dataset$MI_identity_R, "MI_identity_R")
  dataframe <- International_compare(dataframe, dataset$MH_needs, "MH_needs")
  dataframe <- International_compare(dataframe, dataset$MH_needs_met, "MH_needs_met")
  dataframe <- International_compare(dataframe, dataset$MH_training, "MH_training")
  dataframe <- International_compare(dataframe, dataset$MH_academic_impact, "MH_academic_impact")
  dataframe <- International_compare(dataframe, dataset$Belonging_total, "Belonging_total")
  return(dataframe[-1,])
}

##########################################################################################################################
# Compare Fist-Gen
##########################################################################################################################

first_gen_compare <- function(dataframe, c, cName){
  dataframe <- rbind(dataframe, to_table_general(1, "First_Gen", dataset$First_gen, c, cName))
  dataframe <- rbind(dataframe, to_table_general(2, "Not_First_Gen", dataset$First_gen, c, cName))
  dataframe <- rbind(dataframe, to_table(1,2,"First_Gen", "Not_First_Gen",dataset$First_gen,c, cName))
  return(dataframe)
}

first_gen_compile <- function(dataframe){
  dataframe <- first_gen_compare(dataframe, dataset$Overall_MH, "Overall_MH")
  dataframe <- first_gen_compare(dataframe, dataset$Overall_stress, "Overall_stress")
  dataframe <- first_gen_compare(dataframe, dataset$Depression_total, "Depression_total")
  dataframe <- first_gen_compare(dataframe, dataset$Anxiety_total, "Anxiety_total")
  dataframe <- first_gen_compare(dataframe, dataset$MHCSF_total, "MHCSF_total")
  dataframe <- first_gen_compare(dataframe, dataset$HDX_MH_impact, "HDX_MH_impact")
  dataframe <- first_gen_compare(dataframe, dataset$MI_identity_R, "MI_identity_R")
  dataframe <- first_gen_compare(dataframe, dataset$MH_needs, "MH_needs")
  dataframe <- first_gen_compare(dataframe, dataset$MH_needs_met, "MH_needs_met")
  dataframe <- first_gen_compare(dataframe, dataset$MH_training, "MH_training")
  dataframe <- first_gen_compare(dataframe, dataset$MH_academic_impact, "MH_academic_impact")
  dataframe <- first_gen_compare(dataframe, dataset$Belonging_total, "Belonging_total")
  return(dataframe[-1,])
}

##########################################################################################################################
# Compare Major
##########################################################################################################################

Major_compare <- function(dataframe, c, cName){
  dataframe <- rbind(dataframe, to_table_general(1, "Humanities", dataset$Humanities, c, cName))
  dataframe <- rbind(dataframe, to_table_general(1, "Natural_Science", dataset$Natural_sciences, c, cName))
  dataframe <- rbind(dataframe, to_table_general(1, "Social_Science", dataset$Social_sciences, c, cName))
  dataframe <- rbind(dataframe, to_table_general(1, "Interdisciplinary", dataset$Interdisciplinary, c, cName))
  dataframe <- rbind(dataframe, to_table_general(1, "Undeclared", dataset$Undeclared, c, cName))
  dataframe <- rbind(dataframe, to_table_different("Humanities", "Natural_Science", dataset$Humanities, 
                                                   dataset$Natural_sciences, c, cName))
  dataframe <- rbind(dataframe, to_table_different("Humanities", "Social_Science", dataset$Humanities, 
                                                   dataset$Social_sciences, c, cName))
  dataframe <- rbind(dataframe, to_table_different("Social_Science", "Natural_Science", dataset$Social_sciences, 
                                                   dataset$Natural_sciences, c, cName))
  return(dataframe)
}

Major_compile <- function(dataframe){
  dataframe <- Major_compare(dataframe, dataset$Overall_MH, "Overall_MH")
  dataframe <- Major_compare(dataframe, dataset$Overall_stress, "Overall_stress")
  dataframe <- Major_compare(dataframe, dataset$Depression_total, "Depression_total")
  dataframe <- Major_compare(dataframe, dataset$Anxiety_total, "Anxiety_total")
  dataframe <- Major_compare(dataframe, dataset$MHCSF_total, "MHCSF_total")
  dataframe <- Major_compare(dataframe, dataset$HDX_MH_impact, "HDX_MH_impact")
  dataframe <- Major_compare(dataframe, dataset$MI_identity_R, "MI_identity_R")
  dataframe <- Major_compare(dataframe, dataset$MH_needs, "MH_needs")
  dataframe <- Major_compare(dataframe, dataset$MH_needs_met, "MH_needs_met")
  dataframe <- Major_compare(dataframe, dataset$MH_training, "MH_training")
  dataframe <- Major_compare(dataframe, dataset$MH_academic_impact, "MH_academic_impact")
  dataframe <- Major_compare(dataframe, dataset$Belonging_total, "Belonging_total")
  return(dataframe[-1,])
}



##########################################################################################################################
# Compare Disability
##########################################################################################################################

disability_compare <- function(dataframe, c, cName){
  dataframe <- rbind(dataframe, to_table_general(1, "disability", dataset$Disability_status, c, cName))
  dataframe <- rbind(dataframe, to_table_general(2, "No_disability", dataset$Disability_status, c, cName))
  dataframe <- rbind(dataframe, to_table(1,2,"disability", "No_disability",dataset$Disability_status,c, cName))
  return(dataframe)
}

disability_compile <- function(dataframe){
  dataframe <- disability_compare(dataframe, dataset$Overall_MH, "Overall_MH")
  dataframe <- disability_compare(dataframe, dataset$Overall_stress, "Overall_stress")
  dataframe <- disability_compare(dataframe, dataset$Depression_total, "Depression_total")
  dataframe <- disability_compare(dataframe, dataset$Anxiety_total, "Anxiety_total")
  dataframe <- disability_compare(dataframe, dataset$MHCSF_total, "MHCSF_total")
  dataframe <- disability_compare(dataframe, dataset$HDX_MH_impact, "HDX_MH_impact")
  dataframe <- disability_compare(dataframe, dataset$MI_identity_R, "MI_identity_R")
  dataframe <- disability_compare(dataframe, dataset$MH_needs, "MH_needs")
  dataframe <- disability_compare(dataframe, dataset$MH_needs_met, "MH_needs_met")
  dataframe <- disability_compare(dataframe, dataset$MH_training, "MH_training")
  dataframe <- disability_compare(dataframe, dataset$MH_academic_impact, "MH_academic_impact")
  dataframe <- disability_compare(dataframe, dataset$Belonging_total, "Belonging_total")
  return(dataframe[-1,])
}

#####################################################################################
# BOXPLOTS
#####################################################################################

boxplot(dataset$HDX_MH_impact[dataset$Year==1],dataset$HDX_MH_impact[dataset$Year==2],
        dataset$HDX_MH_impact[dataset$Year==3],dataset$HDX_MH_impact[dataset$Year==4], 
        main="Hendrix Mental Health Impact", names=c("Freshmen", "Sophomores","Juniors","Seniors"))


boxplot(dataset$MHCSF_total[dataset$Disability_status==1],dataset$MHCSF_total[dataset$Disability_status==2],
        main="Mental Health Contiuum Short Form ", names=c("Disabled", "Able Bodied"))

boxplot(dataset$Belonging_total[dataset$First_gen==1],dataset$Belonging_total[dataset$First_gen==2],
        main="Total Belonging ", names=c("First Generation", "Not First Generation"))

boxplot(dataset$Overall_stress[dataset$Sexual_orientation==9],dataset$Overall_stress[dataset$Sexual_orientation==2],
        dataset$Overall_stress[dataset$Sexual_orientation==3],dataset$Overall_stress[dataset$Sexual_orientation==4],
        dataset$Overall_stress[dataset$Sexual_orientation==5],dataset$Overall_stress[dataset$Sexual_orientation==6],
        dataset$Overall_stress[dataset$Sexual_orientation==1],
        dataset$Overall_stress[dataset$Sexual_orientation==7],dataset$Overall_stress[dataset$Sexual_orientation==0],
        main="Overall Stress", names=c("Heterosexual", "Bisexual", "Gay", "Lesbian","Pansexual","Queer", "Asexual",
                                         "Questioning","Other"), mar = c(5, 2, 2, 2) , las = 2)

boxplot(dataset$MH_needs[dataset$Sexual_orientation==9],dataset$MH_needs[dataset$Sexual_orientation!=9],
        main="Mental Health Needs", names=c("Heterosexual", "LGBTQA+"))

boxplot(dataset$HDX_MH_impact[dataset$Gender==1],dataset$HDX_MH_impact[dataset$Gender==2],dataset$HDX_MH_impact[dataset$Gender==3],
        main="HDX MH Impact", names=c("Male", "Female", "Other"))

boxplot(dataset$MHCSF_total[dataset$SESrung ==1],dataset$MHCSF_total[dataset$SESrung ==2],
        dataset$MHCSF_total[dataset$SESrung ==3],dataset$MHCSF_total[dataset$SESrung ==4],
        dataset$MHCSF_total[dataset$SESrung ==5],dataset$MHCSF_total[dataset$SESrung ==6],
        dataset$MHCSF_total[dataset$SESrung ==7],dataset$MHCSF_total[dataset$SESrung ==8],
        dataset$MHCSF_total[dataset$SESrung ==9],dataset$MHCSF_total[dataset$SESrung ==10],
        main="MHCSF Total", names = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), xlab=("SES Rung"))

boxplot(dataset$Overall_MH[dataset$Religious_affiliation ==1],dataset$Overall_MH[dataset$Religious_affiliation ==2],
        dataset$Overall_MH[dataset$Religious_affiliation ==4],
        dataset$Overall_MH[dataset$Religious_affiliation ==5],
        dataset$Overall_MH[dataset$Religious_affiliation ==7],
        dataset$Overall_MH[dataset$Religious_affiliation ==9],
        dataset$Overall_MH[dataset$Religious_affiliation ==10],
        dataset$Overall_MH[dataset$Religious_affiliation ==12],
        dataset$Overall_MH[dataset$Religious_affiliation ==11], las=2,
        main="Overall Mental Health", names = c("Agnostic", "Athiest", "Catholic", "Christian", "Jewish",
                                      "Muslim", "No Pref", "Other", "Multiple"))

par(mar=c(8,6,4,2)+0.1)
boxplot(dataset$MH_needs[dataset$Humanities==1],
        dataset$MH_needs[dataset$Social_sciences==1],
        dataset$MH_needs[dataset$Natural_sciences==1],
        dataset$MH_needs[dataset$Interdisciplinary==1],
        dataset$MH_needs[dataset$Undeclared==1], las =2,
        main="Mental Health Needs", names=c("Humanities", "Social Sciences", "Natural Sciences", "Interdisciplinary",
                                            "Undeclared"))

boxplot(dataset$MH_needs[dataset$Trans==1],dataset$MH_needs[dataset$Trans!=1],
        main="Mental Health Needs", names=c("Trans", "Cis"))




boxplot(dataset$Belonging_total[dataset$Race ==2],
        dataset$Belonging_total[dataset$Race ==3],
        dataset$Belonging_total[dataset$Race ==4],
        dataset$Belonging_total[dataset$Race ==5],
        dataset$Belonging_total[dataset$Race ==7],
        dataset$Belonging_total[dataset$Race ==8], las = 2,
        main="Belonging Total", names = c("Asian", "African American", "Hispanic", "Middle Eastern",
                                      "Caucasian", "Multiracial"))

