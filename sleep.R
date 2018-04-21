
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
# Sleep
#################################################################################

# typical hours of sleep per night (open-ended)
min(dataset$Hours_sleep, na.rm=TRUE) #minimum
max(dataset$Hours_sleep, na.rm=TRUE) #maximum
mean(dataset$Hours_sleep, na.rm=TRUE) 
var(dataset$Hours_sleep, na.rm = TRUE)
std(dataset$Hours_sleep)
response_rate(dataset$Hours_sleep)

#Sleep Quality (Scale: 4=very good - 1=very bad)
min(dataset$Sleep_quality_R, na.rm=TRUE) #minimum
max(dataset$Sleep_quality_R, na.rm=TRUE) #maximum
mean(dataset$Sleep_quality_R, na.rm=TRUE) 
var(dataset$Sleep_quality_R, na.rm = TRUE)
std(dataset$Sleep_quality_R)
response_rate(dataset$Sleep_quality_R)
boxplot(dataset$Sleep_quality_R)

#Sleep Hygiene (Scale: 14=very good - 70=very bad)
min(dataset$SHI_total, na.rm=TRUE) #minimum
max(dataset$SHI_total, na.rm=TRUE) #maximum
mean(dataset$SHI_total, na.rm=TRUE) 
var(dataset$SHI_total, na.rm = TRUE)
std(dataset$SHI_total)
response_rate(dataset$SHI_total)
boxplot(dataset$SHI_total)

# Between total Sleep Hygiene and sleep quality
cor(dataset$SHI_total, dataset$Sleep_quality, use="complete.obs", method="pearson")
cov(dataset$SHI_total, dataset$Sleep_quality, use="complete.obs", method="pearson")

# Between total Sleep Hygiene and feeling sleepy during the day
cor(dataset$SHI_total, dataset$Sleep_hygiene15, use="complete.obs", method="pearson")
cov(dataset$SHI_total, dataset$Sleep_hygiene15, use="complete.obs", method="pearson")

# Between total Sleep Hygiene and worry about sleep
cor(dataset$SHI_total, dataset$Sleep_hygiene16, use="complete.obs", method="pearson")
cov(dataset$SHI_total, dataset$Sleep_hygiene16, use="complete.obs", method="pearson")

# Between total Sleep Hygiene and feeling more moody now than before
cor(dataset$SHI_total, dataset$Sleep_hygiene17, use="complete.obs", method="pearson")
cov(dataset$SHI_total, dataset$Sleep_hygiene17, use="complete.obs", method="pearson")

# Between total Sleep Hygiene and feeling like it takes more effort to get things done
cor(dataset$SHI_total, dataset$Sleep_hygiene18, use="complete.obs", method="pearson")
cov(dataset$SHI_total, dataset$Sleep_hygiene18, use="complete.obs", method="pearson")

# Between total Sleep Hygiene and having more trouble paying attention and thinking than before
cor(dataset$SHI_total, dataset$Sleep_hygiene19, use="complete.obs", method="pearson")
cov(dataset$SHI_total, dataset$Sleep_hygiene19, use="complete.obs", method="pearson")

