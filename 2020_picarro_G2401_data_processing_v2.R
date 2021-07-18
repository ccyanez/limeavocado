# Notes 
# this script will be used for pre-processing of mobile lab G2401 data
# it will correct time offsets, calibrate, merge GPS and weather data and perform exclusions 
# this script depends on the file logs.xlsx, which has a record of route details
# two options for calibration -- one point or two point (uncomment the one you want to use)

# libraries ---------------------------------------------------------------
library(data.table)
library(tidyverse)
library(lubridate)
library(stringr)
library(dplyr)
library(readxl)

# functions ---------------------------------------------------------------
rm(list = ls())

subset_calib <- function(t1, t2) 
{ 
  # create plot of this time range to select final calibration start and end 
  temp <- subset(picarro_v2, TIMESTAMP >= t1 & TIMESTAMP <= t2)
  print("Select the beginning and end of the calibration on the plot, then click Finish")
  plot(temp$TIMESTAMP, temp$CO2_dry)
  rows <- identify(temp$TIMESTAMP, temp$CO2_dry)
  act_calib_times <- temp[rows,TIMESTAMP]
  # subset the data
  sub <- subset(picarro_v2, TIMESTAMP >= act_calib_times[1] & TIMESTAMP <= act_calib_times[2])
  
  return(sub)
}

# Input route of interest -------------------------------------------------
routeID <- "20210319"
gen <- read_excel("~cindyyanez/UCR LIME AVOCADO/Data/logs.xlsx", sheet = 'GENERAL') %>% filter(ID == routeID)
yr <- gen$YEAR
mon <- gen$MONTH
day <- gen$DAY
issue <- gen$ISSUE
cal <- gen$CAL

# Get files ---------------------------------------------------------------

path <- "~cindyyanez/UCR LIME AVOCADO/Data/raw/Picarro G2401/"
picarro_raw <- list.files(paste(path,yr,"/",mon,"/",day,sep=""), pattern = "*CFKADS2306-", full.names = TRUE)
if (issue == "yes") {
  # only works if the next day is in the same month
  nextDay <- list.files(paste(path,yr,"/",mon,"/",as.numeric(day)+1,sep=""), pattern = "*CFKADS", full.names = TRUE)
  # nextDay <- list.files(paste(path,yr,"/","08","/","01",sep=""), pattern = "*CFKADS2306-", full.names = TRUE) # temporary solution
  picarro_raw <- c(picarro_raw,nextDay)
} 
print(paste(length(picarro_raw), "files found"))


# Bind the files ----------------------------------------------------------
# Load and bind all methane picarro data sets in UTC time
picarro_v1 <- rbindlist(lapply(picarro_raw,fread, sep=" ", fill=T)) #(level 1)
picarro_v1$TIMESTAMP <- as.POSIXct(paste(picarro_v1$DATE, picarro_v1$TIME), tz="UTC", format="%Y-%m-%d %H:%M:%S") #combines DATE and TIME columns
picarro_v1$CH4 <- picarro_v1$CH4/1000
picarro_v1$CH4_dry <- picarro_v1$CH4_dry/1000
picarro_v2 <- picarro_v1[, -c(1:17,23:26,28:30)] #delete unwanted columns
picarro_v2 <- picarro_v2[,c(7,1,2,3,4,5,6)] # sorts columns so timestamp is first
picarro_v2 <- picarro_v2[, lapply(.SD, round, 2), TIMESTAMP] #round to two decimal places all numeric columns


# Correct time offset -----------------------------------------------------
offset <- read_excel("~cindyyanez/UCR LIME AVOCADO/Data/logs.xlsx", sheet = 'TIMEOFFSET') %>% filter(ID == routeID)
picarro_v2$TIMESTAMP <- picarro_v2$TIMESTAMP - seconds(offset$G2401)

# Save Level 1 and Level 2 file versions ----------------------------------

#Make a  csv file of level 1 version of exported picarro data; 
write.csv(picarro_v1, file=paste("~/UCR LIME AVOCADO/Data/processed/Level01/Picarro G2401/picarro-G2401-",yr,mon,day,"_v1.csv", sep=""))
#Make a  csv file of level 2 version of exported picarro data; 
write.csv(picarro_v2, file=paste("~/UCR LIME AVOCADO/Data/processed/Level02/Picarro G2401/picarro-G2401-",yr,mon,day,"_v2.csv", sep=""))


# Read GPS and weather data -----------------------------------------------

gps_v1 <- read.csv(paste("~/UCR LIME AVOCADO/Data/processed/datalogger/gps_", routeID, ".csv", sep =""), header=T, row.names = 1)
gps_v1$TIMESTAMP <- as.POSIXct(paste(gps_v1$TIMESTAMP), tz="UTC", format="%Y-%m-%d %H:%M:%S")
weather_v1 <- read.csv(paste("~/UCR LIME AVOCADO/Data/processed/datalogger/weather_", routeID, ".csv", sep = ""), header=T, row.names=1)
weather_v1$TIMESTAMP <- as.POSIXct(paste(weather_v1$TIMESTAMP), tz="UTC", format="%Y-%m-%d %H:%M:%S")

# # correct time offset on data logger
# gps_v1$TIMESTAMP <- gps_v1$TIMESTAMP - seconds(offset$Datalogger)
# weather_v1$TIMESTAMP <- weather_v1$TIMESTAMP - seconds(offset$Datalogger)

# Merge GPS, weather, and Picarro data ------------------------------------

picarro_gps = merge(picarro_v2, gps_v1, all=FALSE) #merges picarro and gps by TIMESTAMP (UTC)
picarro_gps_weather = merge(picarro_gps, weather_v1, all=FALSE) #merges picarro, gps, and weather by TIMESTAMP (UTC)

# Subset HIGH calibration data --------------------------------------------

highcal <- read_excel("~cindyyanez/UCR LIME AVOCADO/Data/logs.xlsx", sheet = "HIGHCALIBRATION") %>% filter(ID == routeID)

# MORNING HIGH DATA
if (strftime(highcal$UTC1START, format="%H:%M:%S") != "00:00:00") {
  print("Now we will subset the first (AM) high standard data and store it as 'high1'")
  high1 <- subset_calib(highcal$UTC1START, highcal$UTC1END)
  print("Done!")
}

# AFTERNOON HIGH DATA
if (strftime(highcal$UTC2START, format="%H:%M:%S") != "00:00:00") {
  print("Now we will subset the second (PM) high standard data and store it as 'high2'")
  high2 <- subset_calib(highcal$UTC2START, highcal$UTC2END)
  print("Done!")
}

# save information about high calibration
if (exists("high1")) {
  high <- rbind(high1, high2)
  high_summary <- sapply(high, function(high) c( "STD" = sd(high),
                                                 "Mean" = mean(high, na.rm = TRUE)))

  high_summary <- as.data.frame(high_summary)
  print("Summary of high calibration subset: ")
  print(high_summary)
  write.csv(high_summary, file=paste("~/UCR LIME AVOCADO/Data/processed/calibration_summary/picarro-G2401-",yr,mon,day,"_high_calibration.csv",sep=""))
  print("This summary has been saved as a .csv file")
}

# Subset LOW calibration data ---------------------------------------------

lowcal <- read_excel("~cindyyanez/UCR LIME AVOCADO/Data/logs.xlsx", sheet = "LOWCALIBRATION") %>% filter(ID == routeID)

# MORNING LOW DATA
if (strftime(lowcal$UTC1START, format="%H:%M:%S") != "00:00:00") {
  print("Now we will subset the first (AM) low standard data and store it as 'low1'")
  low1 <- subset_calib(lowcal$UTC1START, lowcal$UTC1END)
  print("Done!")
}

# AFTERNOON LOW DATA
if (strftime(lowcal$UTC2START, format="%H:%M:%S") != "00:00:00") {
  print("Now we will subset the second (PM) low standard data and store it as 'low2'")
  low2 <- subset_calib(lowcal$UTC2START, lowcal$UTC2END)
  print("Done!")
}

# save information about low calibration
if (exists("low1")) {
  low <- rbind(low1, low2)
  low_summary <- sapply(low, function(low) c( "STD" = sd(low),
                                              "Mean" = mean(low, na.rm = TRUE)))

  low_summary <- as.data.frame(low_summary)
  print("Summary of low calibration subset: ")
  print(low_summary)
  # write.csv(low_summary, file=paste("~/UCR LIME AVOCADO/Data/processed/calibration_summary/picarro-G2401-",yr,mon,day,"_low_calibration.csv",sep=""))
  print("This summary has been saved as a .csv file")
}

# Special Case ------------------------------------------------------------

# Only need this section if using a different day's data to calibrate 

# # if yes (for high)
if (!is.na(highcal$EXTFILE)) {
  file <- highcal$EXTFILE
  high_summary <- read.csv(file, header=T, row.names = 1)
  high_summary <- as.data.frame(high_summary)
}
# 
# # if yes (for low)
if (!is.na(lowcal$EXTFILE)) {
  file <- lowcal$EXTFILE
  low_summary <- read.csv(file, header=T, row.names = 1)
  low_summary <- as.data.frame(low_summary)
}

# Apply calibration -------------------------------------------------------

standards <- read_excel("~cindyyanez/UCR LIME AVOCADO/Data/logs.xlsx", sheet = "STANDARDS") %>% filter(ID == routeID)

if (cal == 2) {
  # correct CH4 dry: two-point calibration
  lowCH4 <- standards$CH4LOW
  highCH4 <- standards$CH4HIGH
  known_measured <- data.frame("Known" = c(lowCH4,highCH4),
                               "Measured" = c(low_summary["Mean", "CH4_dry"], high_summary["Mean", "CH4_dry"]))
  plot(known_measured$Measured, known_measured$Known, main = "CH4 Calibration")
  fit <- lm(known_measured$Known ~ known_measured$Measured)
  Intercept<-coef(fit)["(Intercept)"]
  Slope<-coef(fit)["known_measured$Measured"]
  picarro_gps_weather$CH4_dry <- Slope*picarro_gps_weather$CH4_dry + Intercept
  
  ## correct CO2 dry: two-point calibration
  lowCO2 <- standards$CO2LOW
  highCO2 <- standards$CO2HIGH
  known_measured <- data.frame("Known" = c(lowCO2,highCO2),
                               "Measured" = c(low_summary["Mean", "CO2_dry"], high_summary["Mean", "CO2_dry"]))
  plot(known_measured$Measured, known_measured$Known, main = "CO2 Calibration")
  fit <- lm(known_measured$Known ~ known_measured$Measured)
  Intercept<-coef(fit)["(Intercept)"]
  Slope<-coef(fit)["known_measured$Measured"]
  picarro_gps_weather$CO2_dry <- Slope*picarro_gps_weather$CO2_dry + Intercept
  
  ## correct CO: two-point calibration
  lowCO <- standards$COLOW
  highCO <- standards$COHIGH
  known_measured <- data.frame("Known" = c(lowCO,highCO),
                               "Measured" = c(low_summary["Mean", "CO"], high_summary["Mean", "CO"]))
  plot(known_measured$Measured, known_measured$Known, main = "CO Calibration")
  fit <- lm(known_measured$Known ~ known_measured$Measured)
  Intercept<-coef(fit)["(Intercept)"]
  Slope<-coef(fit)["known_measured$Measured"]
  picarro_gps_weather$CO <- Slope*picarro_gps_weather$CO + Intercept
  
} else if (cal == 1) {
  ## correct CO: one-point calibration
  known_lowCO <- standards$COLOW
  measured_lowCO <- low_summary["Mean","CO"]
  picarro_gps_weather$CO <- picarro_gps_weather$CO*(known_lowCO/measured_lowCO)

  ## correct CH4_dry: one-point calibration
  known_lowCH4 <- standards$CH4LOW
  measured_low <- low_summary["Mean","CH4_dry"]
  picarro_gps_weather$CH4_dry <- picarro_gps_weather$CH4_dry*(known_lowCH4/measured_low)

  ## correct CO2_dry: one-point calibration
  known_lowCO2 <- standards$CO2LOW
  measured_low <- low_summary["Mean","CO2_dry"]
  picarro_gps_weather$CO2_dry <- picarro_gps_weather$CO2_dry*(known_lowCO2/measured_low)
  
}

# Exclusions --------------------------------------------------------------
# gen <- read_excel("~cindyyanez/UCR LIME AVOCADO/Data/logs.xlsx", sheet = 'GENERAL') %>% filter(ID == "20210226")
driveStart <- gen$STARTUTC
driveEnd <- gen$ENDUTC
picarro_gps_weather <- subset(picarro_gps_weather, TIMESTAMP >= driveStart & TIMESTAMP <= driveEnd)

# stationary data, only save the times we were parked at Arvin's house 
##picarro_gps_weather <- subset(picarro_gps_weather, Latitude == 33.848 & Speed == 0)

# Save corrected data (V3) -----------------------------------------------------
picarro_v3 <- picarro_gps_weather[, -c(2,4)] #delete unwanted columns
picarro_v3[,c(2:16)] <- lapply(picarro_v3[,c(2:16)], as.numeric)
# picarro_v3 <- picarro_v3[, lapply(.SD, round, 2), TIMESTAMP] #round to two decimal places all numeric columns

#Make a csv file of level 3 version of exported picarro data;
write.csv(picarro_v3, file=paste("~/UCR LIME AVOCADO/Data/processed/calibrated/Picarro G2401/twopointcalibration/",yr,"/picarro-G2401-",yr,mon,day,"_v3_2p.csv", sep=""))
print("Calibrated data has been saved as a csv.")

# Time-sync the data ------------------------------------------------------

# test <- read.csv(paste("~/UCR LIME AVOCADO/Data/processed/calibrated/Picarro G2401/twopointcalibration/",yr,"/picarro-G2401-",yr,mon,day,"_v3_2p.csv", sep=""))
# test[,c(1,3:17)] <- lapply(test[,c(1,3:17)], as.numeric)
# test$TIMESTAMP <- as.POSIXct(test$TIMESTAMP)
picarro_v4 <- aggregate(. ~ group, transform(picarro_v3, group = cut(TIMESTAMP, breaks = "5 secs")), FUN = mean)
# picarro_v4_sd <- aggregate(. ~ group, transform(test, group = cut(TIMESTAMP, breaks = "5 secs")), FUN = sd)
picarro_v4 <- picarro_v4[,-2]
colnames(picarro_v4)[1] <- "TIMESTAMP"

write.csv(picarro_v4, file = paste("~/UCR LIME AVOCADO/Data/processed/time-synched/picarro-G2401_",routeID,"_2pcalibrated_5sec.csv", sep =""))
