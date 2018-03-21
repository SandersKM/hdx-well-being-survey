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

#frequeny of people who answered "yes"
#percentage with scores above 0:
freq_bool <- function(col){
  tbl <- table(na.omit(col))
  return(sum(tbl[names(tbl)==1])/sum(tbl))
}

#################################################################################
# Use of Health Services
#################################################################################

# Frequency who have vs. have not received counseling 
freq_bool(dataset$Ever_counseling)

#	Frequency who are vs. are not currently receiving counseling 
freq_bool(dataset$Current_counseling)

#	Frequency who have vs. have not used mental health services or counseling on campus 
freq_bool(dataset$Used_campus_MH_services)

#	Frequency  who have vs. have not used physical health services on campus 
freq_bool(dataset$Used_campus_PH_services)

# Perception of of MH/counciling services availible at HDX
Availability_MH_services_tbl <- table(na.omit(dataset$Availability_MH_services))
# Mostly negative opinions - 1
sum(Availability_MH_services_tbl[names(Availability_MH_services_tbl)==1])/sum(Availability_MH_services_tbl)
# Mix of negative and positive opinions - 2
sum(Availability_MH_services_tbl[names(Availability_MH_services_tbl)==2])/sum(Availability_MH_services_tbl)
# Mostly positive opinions - 3
sum(Availability_MH_services_tbl[names(Availability_MH_services_tbl)==3])/sum(Availability_MH_services_tbl)
# I have no opinions and haven’t heard others’ opinions - 4
sum(Availability_MH_services_tbl[names(Availability_MH_services_tbl)==4])/sum(Availability_MH_services_tbl)
# Ok histogram of results...
hist(dataset$Availability_MH_services, breaks = c(0,1,2,3,4), xlab="Availability of MH services")

# Perception of of PH/counciling services availible at HDX
Availability_PH_services_tbl <- table(na.omit(dataset$Availability_PH_services))
# Mostly negative opinions - 1
sum(Availability_PH_services_tbl[names(Availability_PH_services_tbl)==1])/sum(Availability_PH_services_tbl)
# Mix of negative and positive opinions - 2
sum(Availability_PH_services_tbl[names(Availability_PH_services_tbl)==2])/sum(Availability_PH_services_tbl)
# Mostly positive opinions - 3
sum(Availability_PH_services_tbl[names(Availability_PH_services_tbl)==3])/sum(Availability_PH_services_tbl)
# I have no opinions and haven’t heard others’ opinions - 4
sum(Availability_PH_services_tbl[names(Availability_PH_services_tbl)==4])/sum(Availability_PH_services_tbl)
# Ok histogram of results...
hist(dataset$Availability_PH_services, breaks = c(0,1,2,3,4), xlab="Availability of PH services")

# In the last year, I needed MH help (1=stongly disagree - 6 = strongly agree)
mean(dataset$MH_needs_R, na.rm=TRUE) 
var(dataset$MH_needs_R, na.rm = TRUE)
std(dataset$MH_needs_R)
response_rate(dataset$MH_needs_R)
boxplot(dataset$MH_needs_R)

# In the last year, I felt my MH needs were met (1=stongly disagree - 6 = strongly agree)
mean(dataset$MH_needs_met_R, na.rm=TRUE) 
var(dataset$MH_needs_met_R, na.rm = TRUE)
std(dataset$MH_needs_met_R)
response_rate(dataset$MH_needs_met_R)
boxplot(dataset$MH_needs_met_R)

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