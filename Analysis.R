
library(readxl) #library used to import Excel data
# import the Hendrix Well Being Survey Data (change location for your own HWBS Data)
dataset <- read_excel("C:/Users/kates/Desktop/HWBS_STUDENTS_2017_condensed.xlsx")
rownum <- 531 #total number of responses recorded
mean(dataset$Overall_MH_R, na.rm=TRUE) #average mental health (scale:5)
(rownum - sum(is.na(dataset$Overall_MH_R)))/ rownum #response rate

