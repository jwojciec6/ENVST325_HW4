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
       y = "Temperature degrees Celsius") 

##Prompt 2

