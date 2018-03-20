# import the Hendrix Well Being Survey Data (change location for your own HWBS Data)
dataset <- read_excel("C:/Users/kates/Desktop/HWBS_STUDENTS_2017_condensed.xlsx")
rownum <- 531 #total number of responses recorded

#function for determining the response rate (% data that was not NA)
response_rate <- function(col){
  rr <- (rownum - sum(is.na(col)))/ rownum 
  return(rr)
}

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
boxplot(dataset$Sleep_quality)

#Sleep Hygiene (Scale: 14=very good - 70=very bad)
SHI_total_min <- min(dataset$SHI_total, na.rm=TRUE) #minimum
SHI_total_max <- max(dataset$SHI_total, na.rm=TRUE) #maximum
SHI_total_mean <- mean(dataset$SHI_total, na.rm=TRUE) 
SHI_total_var <- var(dataset$SHI_total, na.rm = TRUE)
SHI_total_std <- sqrt(SHI_total_var)
SHI_total_rr <- response_rate(dataset$SHI_total)
boxplot(dataset$SHI_total)
