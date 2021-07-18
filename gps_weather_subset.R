###First, ensure to install all packages listed below

library(data.table)
library(tidyverse)
library(lubridate)
library(stringr)
library(dplyr)
library(readxl)

#--------------------------------------------------GPS data processing --------------------------------------------------
routeID = '20210625'

gps_raw = read.delim(paste('~/UCR LIME AVOCADO/Data/raw/datalogger/2021/',routeID,'/CR1000X_GPS16X.dat',sep=""), sep=",", header = FALSE) #import GPS csv file
gps_v1 <- gps_raw[-c(1,3,4), -c(2,3)] #delete rows & columns
colnames(gps_v1) <- as.character(unlist(gps_v1[1,]))
gps_v1 = gps_v1[-1, ]
gps_v1$TIMESTAMP <- as.POSIXct(paste(gps_v1$TIMESTAMP), tz="UTC", format="%Y-%m-%d %H:%M:%S")
gps_v1[,c(2:8)] <- as.numeric(as.character(unlist(gps_v1[, c(2:8)]))) #convert columns from factor to numeric


#convert latitude and longitude to decimal degrees in a new column
gps_v1 <-
   gps_v1 %>%
    mutate(Latitude_B = Latitude_B / 60,
           Longitude_B = Longitude_B / 60)
gps_v1 <-
    gps_v1 %>%
    mutate(Latitude_A = Latitude_A + Latitude_B,
           Longitude_A = Longitude_A + Longitude_B)
gps_v1 <- gps_v1[, -c(3,5)] #delete columns not used
gps_v1 <- rename(gps_v1, Latitude = Latitude_A, Longitude = Longitude_A)  #rename new column

# subset time period of interest
subset_gps <- subset(gps_v1,
                 TIMESTAMP >= as.POSIXct('2021-06-01 00:00:00',
                                         tz = "US/Pacific") &
                   TIMESTAMP <= as.POSIXct('2021-07-01 00:00:00',
                                           tz = "US/Pacific"))

# quick sketch of the data 
plot(subset_gps$TIMESTAMP, subset_gps$Longitude) 
plot(subset_gps$Longitude, subset_gps$Latitude)

# correct time offset
# offset <- read_excel("~cindyyanez/UCR LIME AVOCADO/Data/logs.xlsx", sheet = 'TIMEOFFSET') %>% filter(ID == routeID)
# subset_gps$TIMESTAMP <- subset_gps$TIMESTAMP - seconds(offset$Datalogger)

# save as csv file
write.csv(subset_gps, file=paste("~/UCR LIME AVOCADO/Data/processed/datalogger/gps_",routeID,".csv", sep ="")) #specify date, month, and day in file name with format YYYYMMDD

#--------------------------------------------------weather station data processing --------------------------------------------------

#import weather file
weather_raw = read.delim(paste('~/UCR LIME AVOCADO/Data/raw/datalogger/2021/',routeID,'/CR1000X_METSENS500.dat',sep=""), sep=",", header=FALSE)
weather_v1 <- weather_raw[-c(1,3,4),-c(2,9,10)] #delete rows & columns
colnames(weather_v1) <- as.character(unlist(weather_v1[1,]))
weather_v1 = weather_v1[-1, ]
weather_v1$TIMESTAMP <- as.POSIXct(paste(weather_v1$TIMESTAMP), tz="UTC", format="%Y-%m-%d %H:%M:%S")

subset_weather <- subset(weather_v1,
                 TIMESTAMP >= as.POSIXct('2021-07-01 00:00:00',
                                         tz = "US/Pacific") &
                   TIMESTAMP <= as.POSIXct('2021-08-01 09:00:00',
                                           tz = "US/Pacific"))

# correct time offset
# offset <- read_excel("~cindyyanez/UCR LIME AVOCADO/Data/logs.xlsx", sheet = 'TIMEOFFSET') %>% filter(ID == routeID)
# subset_weather$TIMESTAMP <- subset_weather$TIMESTAMP - seconds(offset$Datalogger)

write.csv(subset_weather, file=paste("~/UCR LIME AVOCADO/Data/processed/datalogger/weather_",routeID,".csv",sep="")) #specify date, month, and day in file name with format YYYYMMDD

