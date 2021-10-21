## Script for processing of Picarro G2401 data 
## Author: Cindy Yanez
## Last revised: Oct. 1, 2021
# See README.txt file for definition of data levels

# User inputs -------------------------------------------------------------
routeID <- "20190715" # enter routeID for survey
main <- '~/UCR LIME AVOCADO/Data/' # main path to data, all other paths are relative to this one
logFile <- paste(main, 'logs2.xlsx', sep='') # path to log file with metadata
raw_file_path <- paste(main, 'raw/Picarro G2401/', sep = '') # input path to raw Picarro files
v1_file_path <- paste(main, 'processed/Level01/Picarro G2401/', sep='') # output path for Level 1 files
v2_file_path <- paste(main, 'processed/Level02/Picarro G2401/', sep='') # output path for Level 2 files
calSum_file_path <- paste(main, 'processed/calibration_summary/', sep='') # output path for calibration summary files

# Libraries ---------------------------------------------------------------
library(data.table)
library(tidyverse)
library(lubridate)
library(stringr)
library(dplyr)
library(readxl)
library(purrr)
source('~/UCR LIME AVOCADO/Scripts/R/limeavocado_functions.R')

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
calInfo <- read_excel(logFile, sheet = 'CALIBRATIONS') %>% filter(ID == routeID) # load the calibration information
calInfo$T1 <- as.POSIXct(paste(calInfo$DATE, calInfo$T1), tz ='America/Los_Angeles') # convert start times to datetime format
calInfo$T2 <- as.POSIXct(paste(calInfo$DATE, calInfo$T2), tz ='America/Los_Angeles') # convert end times to datetime format

picarro_cals <- picarro_v2[setDT(calInfo), on = .(TIMESTAMP >= T1, TIMESTAMP <= T2), # subsets picarro data that is in calibration times
                   `:=`(STANDARD_ID = STANDARD_ID)] %>% drop_na(STANDARD_ID) # appends STANDARD_ID column

# load known standard values
standards <- read_excel(logFile, sheet = 'STANDARDS') %>% filter(STANDARD_ID %in% calInfo$STANDARD_ID) 
standards <- standards[order(standards$STANDARD_ID),]

# average measured calibration values
measured <- picarro_cals %>% group_by(STANDARD_ID) %>% summarize(across(where(is.numeric), ~mean(.))) %>% # take average by standard_ID
  select(STANDARD_ID, CO2_dry, CO, CH4_dry) %>% # filter just CO2_dry, CO, and CH4_dry
  rename(CH4 = CH4_dry, CO2 = CO2_dry) # changes column names, from here on using dry values for CO2 and CH4
measured <- measured[order(measured$STANDARD_ID),] 

# plot the standard vs measured values
par(mfrow=c(1,3))
plot(standards$CO2, measured$CO2)
plot(standards$CO, measured$CO)
plot(standards$CH4, measured$CH4)

# calculate linear fits for each gas
cal <- merge(standards, measured, by = "STANDARD_ID", suffixes = c(".known",".measured")) %>% # merge known and measured values
  pivot_longer(CO2.known:CH4.measured) %>%  # convert to long format
  separate(name, into=c("species","type")) %>% # get values from column names
  pivot_wider(names_from = type, values_from=value) %>% # go back to wide format
  nest(data = -species) %>%  # nest all values into groups by species
  mutate(reg = map(data, ~lm(known ~ measured, .))) %>% # do the regression for each species
  mutate(intercept = map_dbl(reg, ~coefficients(.)[1]), # get values from the regression
         slope = map_dbl(reg, ~coefficients(.)[2])) %>%
  column_to_rownames(var = "species")
  
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
# delete unwanted columns 
# round numeric columns to 4 decimal places
# write.csv(picarro_v3, file = [insert file path here])


# Future to do list -------------------------------------------------------
# (1) output calibration summary statistics
# (2) convert the calibration section into a function
# (3) make sure the calibration section works for special cases (e.g. only calibrated once, 
#                     only had one tank, using a different day's data to calibrate, etc.)
# (4) Save corrected data

