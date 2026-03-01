#John Wojciechowski
#ENVST 325 HW4
#2/24/2026

install.packages(c("dplyr","ggplot2","lubridate"))
library(dplyr)
library(ggplot2)
library(lubridate)

#inclass work
weather <- read.csv("/cloud/project/activity04/campus_weather.csv",
                    na.strings = "#N/A")
metaDat <- read.csv("/cloud/project/activity04/meter_weather_metadata.csv",
                    na.strings = "#N/A")
sensorLog <- read.csv("/cloud/project/activity04/Sensor log.csv",
                      na.strings = "#N/A")

weather$dateF <- mdy_hm(weather$Date)
weather$dateET <- mdy_hm(weather$Date, tz = "America/New_York")

weatherCheck <- weather %>%
  filter(is.na(weather$dateET))

weather$dateF[2] %--% weather$dateF[3]

int_length(weather$dateF[2] %--% weather$dateF[3])

test <- weather$dateF[1:10]
test
test[-1]

#x is a date vector
timeCheck900 <- function(x){
  intervals <- x[-length(x)] %--% x[-1]
  interval_times <- int_length(intervals)
  intervals[interval_times != 900]
}
timeCheck900(weather$dateF)

soilFiles <- list.files("/cloud/project/activity04/soil")
#set up variable to be used in for loop
soilList <- list()

for(i in 1:length(soilFiles)) {
  soilList[[i]] <- read.csv(paste0("/cloud/project/activity04/soil/", soilFiles[i]))
}

str(soilList)

soilData <- do.call("rbind", soilList)

#calculate moving average 
airMA <- numeric()

for(i in 8:length(weather$AirTemp)) {
  airMA[i] <- mean(weather$AirTemp[(i-7:i)])
}
airMA
weather$airMA <-airMA

head(sensorLog)
##Prompt 1

Jan_temps <- weather %>%
  filter(format(dateF, "%m") == "01" & format(dateF, "%Y") == "2022")

airtemp_jan <- numeric()

for(i in 8:length(Jan_temps$AirTemp)) {
  airtemp_jan[i] <- mean(Jan_temps$AirTemp[(i-7:i)])
}

Jan_temps$rolling_avg <- airtemp_jan

ggplot(Jan_temps, aes(x =dateF)) +
  geom_point(aes(y=AirTemp), color ="black") +
  geom_line(aes(y=rolling_avg), color ="blue") +
  labs(title = "January 2022 Rolling Average Air Temp",
       x = "Date", 
       y = "Temperature (°C)") 

##Prompt 2

mayjune_solar <- weather %>%
  filter(format(dateF, "%m") %in% c("05", "06") & format(dateF, "%Y") == "2021" )

ggplot(mayjune_solar) +
  aes(dateF, SolRad) +
  geom_point() +
  labs(title = "Solar Radiation May-June 2021",
       x ="Date",
       y= "Solar Radiation (W/m^2)")

##looking at the graph there does not appear to be 
## any indication of build up in the sensor readings
## you can follow the consistent peaks and valleys 
## of the day and night cycle some days are lower total 
## but that can probably be attributed to cloudy days 
## Overall the peaks and valleys are consistent throughout this time
## period with no indication of accumulation causing lower or higher readings


##Prompt 3

##Discussed in class, There is problems with the data around 
## daylight savings caused by the gap in time. There are issues with 
## time zone assumptions as we saw the time changed at 5am and not 2am 
## like normal due to the sensor being programmed to switch in a different time zone 
## then where the sensor was located.


##Question 1

##remove below 0 temps and more than 2 degrees imbalance
clean_percip <- weather %>%
  filter(AirTemp >= 0) %>%
  filter(between(XLevel, -2, 2) & between(YLevel, -2, 2))

##sum number of N/A's in precipitation data
sum(is.na(weather$Precip))

##Question 2

##flag for low voltage 
##if under 8500 mV flag with a 1
##if no low voltage flag with a 0
weather$lowvoltage <- ifelse(weather$BatVolt < 8500, 1, 0)


##Question 3

##Checks ranges of data for temperature and solar radation
check_real <- function(data, temp, solar) {
  
  data$temp_flag <- ifelse(data[[temp]] < -25 | data[[temp]] > 35,
                      "Warning", "Ok")
  
  data$solar_flag <- ifelse(data[[solar]] < 0 | data[[solar]] > 1000,
                      "Warning", "Ok")
  
  return(data)
}

#calling function to make two additional flag columns
weather <- check_real(weather, "AirTemp", "SolRad")

##Question 4

##filter out N/As and only Jan-March 2021 data
winter_temps <- weather %>%
  filter(format(dateF, "%m") %in% c("01", "02", "03") & 
           format(dateF, "%Y") == "2021") %>%
  filter(!is.na(AirTemp))

##plot temperature data
ggplot(winter_temps) +
  aes(x = dateF, y = AirTemp) +
  geom_line(color ="cadetblue") +
  labs(title = "Winter Air Temperatures Jan-Mar 2021",
       x = "Date",
       y = "Air Temperature (°C)")

##Question 5

Marapr <- weather %>%
  filter(format(dateF, "%m") %in% c("03", "04") & 
           format(dateF, "%Y") == "2021")

##temperature for under 35 degrees Fahrenheit
under_35 <- 1.67

##make a date only column 
Marapr$date_only <- as.Date(Marapr$dateF)

##get unique dates

unique_dates <- unique(Marapr$date_only)

##calculate if first entry is above 35 degrees Fahrenheit
first_day <- unique_dates[1]
if(any(Marapr$AirTemp[Marapr$date_only == first_day] < under_35, na.rm = TRUE)) {
  Marapr$Precip[Marapr$date_only == first_day] <- NA
}

##Calculate is the rest of the days are valid 
for (i in 2:length(unique_dates)) {
  current_day <- unique_dates[i]
  previous_day <- unique_dates[i-1]
  
  current_low <- any(Marapr$AirTemp[Marapr$date_only == current_day] < under_35, na.rm = TRUE)
  prev_low <- any(Marapr$AirTemp[Marapr$date_only == previous_day] < under_35, na.rm = TRUE)
  
  if(current_low | prev_low) {
    Marapr$Precip[Marapr$date_only == current_day] <- NA
  }
}

##sum number of valid readings and divide by 96 to get daily numbers
sum(!is.na(Marapr$Precip)) /96


##Question 6

soilFiles <- list.files("/cloud/project/activity04/soil")
#set up variable to be used in for loop
soilList <- list()

for(i in 1:length(soilFiles)) {
  soilList[[i]] <- read.csv(paste0("/cloud/project/activity04/soil/", soilFiles[i]))
}

str(soilList)

soilData <- do.call("rbind", soilList)

#parse date for soil data 
soilData$dateF <- ymd_hm(soilData$Timestamp)

#time check function with extra attribute for specified interval
timeCheck <- function(x, time_interval){
  intervals <- x[-length(x)] %--% x[-1]
  interval_times <- int_length(intervals)
  intervals[interval_times != time_interval]
}

##check for hourly intervals in the data
timeCheck(soilData$dateF, 3600)



