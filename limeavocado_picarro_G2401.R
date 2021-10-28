## Script for processing of Picarro G2401 data 
## Author: Cindy Yanez
## Last revised: Oct. 21, 2021
# See README.txt file for definition of data levels

# User inputs -------------------------------------------------------------
routeID <- "20190715" # enter routeID for survey
main <- '/Volumes/Seagate Expansion Drive/ucrlimeavocado/Data/' # main path to data, all other paths are relative to this one
logFile <- paste(main, 'logs2.xlsx', sep='') # path to log file with metadata
raw_file_path <- paste(main, 'raw/Picarro G2401/', sep = '') # input path to raw Picarro files
v1_file_path <- paste(main, 'processed/Level01/Picarro G2401/', sep='') # output path for Level 1 files
v2_file_path <- paste(main, 'processed/Level02/Picarro G2401/', sep='') # output path for Level 2 files
calSum_file_path <- paste(main, 'processed/calibration_summary/', sep='') # output path for calibration summary files
v3_file_path <- paste(main, 'processed/Level03/Picarro G2401/', sep='') # output path for Level 3 files
qaqc_file_path <- paste(main,'QAQC/Picarro G2401/',sep='') # output path for quality control report

# Libraries ---------------------------------------------------------------
library(data.table)
library(tidyverse)
library(lubridate)
library(stringr)
library(dplyr)
library(readxl)
library(purrr)
source('~/UCR LIME AVOCADO/limeavocado/limeavocado_functions.R')
source('~/UCR LIME AVOCADO/limeavocado/get_calib_values.R')
source('~/UCR LIME AVOCADO/limeavocado/calibrate.R')

# Level 1 Processing ------------------------------------------------------------
surveyInfo = read_excel(logFile, sheet = 'GENERAL') %>% filter(ID == routeID) # import general survey information from log file
files = get_files(surveyInfo, in_path = raw_file_path)  # Finds the data files in your computer using survey info
picarro_v1 = create_level_1(files, v1_file_path) # Level 1 data gets exported

# Level 2 Processing ------------------------------------------------------------
offset <- read_excel(logFile, sheet = 'TIMEOFFSET') %>% filter(ID == routeID) # import Picarro G2401 time offset
picarro_v2 <- create_level_2(picarro_v1, offset$G2401, v2_file_path) # Level 2 data gets exported

# Level 3 Processing  ---------------------------------------------
picarro_gps_weather <- merge_picarro_datalogger(picarro_v2, surveyInfo$GPSFILE, surveyInfo$WEATHERFILE) %>% # merge Level 2 data with gps and weather
  select(-c(CH4, CO2)) %>% rename(CH4 = CH4_dry, CO2 = CO2_dry) # from here on, only use the CO2_dry and CH4_dry columns, they are renamed to CO2 and CH4

# CALIBRATE ---------------------------------------------------------------
calInfo <- read_excel(logFile, sheet = 'CALESTIMATES') %>% filter(ID == routeID) # load the calibration information

# get average measured values
measured <- get_calib_values(calInfo)

# load known standard values
standards <- read_excel(logFile, sheet = 'STANDARDS') %>% filter(STANDARD_ID %in% calInfo$STANDARD_ID) 
standards <- standards[order(standards$STANDARD_ID),]

# plot the standard vs measured values
par(mfrow=c(1,3))
plot(standards$CO2, measured$CO2)
plot(standards$CO, measured$CO)
plot(standards$CH4, measured$CH4)

cal <- calibrate(measured, standards) # calculate coefficients for correction
  
# apply the calibration for each gas
picarro_gps_weather <- mutate(picarro_gps_weather, 
                              CH4_corr = CH4*cal["CH4","slope"] + cal["CH4","intercept"],
                              CO2_corr = CO2*cal["CO2","slope"] + cal["CO2","intercept"],
                              CO_corr = CO*cal["CO","slope"] + cal["CO","intercept"])
# Exclusions --------------------------------------------------------------

# only include driving times (exclude calibrations) 
driveStart <- surveyInfo$STARTUTC
driveEnd <- surveyInfo$ENDUTC
picarro_gps_weather <- subset(picarro_gps_weather, TIMESTAMP >= driveStart & TIMESTAMP <= driveEnd)


# Save corrected data  ----------------------------------------------------
picarro_v3 <- picarro_gps_weather[, -c(2,3,5)] #delete uncalibrated gas values
picarro_v3[,c(2:16)] <- lapply(picarro_v3[,c(2:16)], as.numeric)
picarro_v3 <- picarro_v3[, lapply(.SD, round, 4), TIMESTAMP] #round all numeric columns to 4 decimal places
picarro_v3 <- rename(picarro_v3, CH4 = CH4_corr, CO2 = CO2_corr, CO = CO_corr)

#Make a csv file of level 3 version of exported picarro data;
write.csv(picarro_v3, file=paste(v3_file_path, "picarro-G2401-",routeID,"_v3.csv", sep=""), row.names = FALSE)
print("Calibrated data has been saved as a csv.")

# Quality Control Report --------------------------------------------------
qc <- list("Route ID" = routeID,
           "Files found" = length(files),
           "Level 1 length" = length(picarro_v1),
           "Level 2 length" = length(picarro_v2),
           "Level 3 length" = length(picarro_v3),
           "Missing values" = sum(is.na(picarro_v3)),
           "Negative CO2 values" = sum(picarro_v3$CO2 < 0),
           "Negative CO values" = sum(picarro_v3$CO < 0),
           "Negative CH4 values" = sum(picarro_v3$CH4 < 0),
           "Minimum CO2 (ppm)" = min(picarro_v3$CO2),
           "Minimum CO (ppb)" = min(picarro_v3$CO),
           "Minimum CH4 (ppm)" = min(picarro_v3$CH4),
           "Maximum CO2 (ppm)" = max(picarro_v3$CO2),
           "Maximum CO (ppb)" = max(picarro_v3$CO),
           "Maximum CH4 (ppm)" = max(picarro_v3$CH4)
)
qc

par(mfrow=c(3,1))
plot(picarro_v3$TIMESTAMP, picarro_v3$CO2)
plot(picarro_v3$TIMESTAMP, picarro_v3$CO)
plot(picarro_v3$TIMESTAMP, picarro_v3$CH4)

# Future to do list -------------------------------------------------------
# output calibration summary statistics

