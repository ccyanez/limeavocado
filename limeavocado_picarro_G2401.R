## Script for processing of Picarro G2401 data 
## Author: Cindy Yanez
## Last revised: February 6, 2022
# See README.txt file for definition of data levels

# User inputs -------------------------------------------------------------
rm(list = ls())
routeID <- "20210715" # enter routeID for survey

# setwd('E:/')

# main <- '/Volumes/cindrive/ucrlimeavocado/Data/' # main path to data, all other paths are relative to this one
main <- 'E:/ucrlimeavocado/Data/'
logFile <- paste(main, 'logs2.xlsx', sep='') # path to log file with metadata
raw_file_path <- paste(main, 'raw/Picarro G2401/', sep = '') # input path to raw Picarro files
v1_file_path <- paste(main, 'processed/Level01/Picarro G2401/', sep='') # output path for Level 1 files
v2_file_path <- paste(main, 'processed/Level02/Picarro G2401/', sep='') # output path for Level 2 files
calSum_file_path <- paste(main, 'processed/calibration_summary/', sep='') # output path for calibration summary files
v3_file_path <- paste(main, 'processed/Level03/Picarro G2401/', sep='') # output path for Level 3 files
report_file_path <- paste(main,'reports/Picarro G2401/',sep='') # output path for processing report
qaqc_file_path <- paste(main, 'processed/qaqc/Picarro G2401/', sep='') # output path for qaqc report

# Libraries ---------------------------------------------------------------
library(data.table)
library(tidyverse)
library(lubridate)
library(stringr)
library(dplyr)
library(readxl)
library(purrr)
library(gdata)
source('C:/Users/cindy/limeavocado/limeavocado_functions.R')
source('C:/Users/cindy/limeavocado/get_calib_values.R')
source('C:/Users/cindy/limeavocado/calibrate.R')
# source('C:/Users/cindy/limeavocado/flagCO.R')

# Level 1 Processing ------------------------------------------------------------
surveyInfo = read_excel(logFile, sheet = 'GENERAL') %>% filter(ID == routeID) # import general survey information from log file
files = get_files(surveyInfo, in_path = raw_file_path)  # Finds the data files in your computer using survey info
picarro_v1 = create_level_1(files, v1_file_path) # Level 1 data gets exported

# Level 2 Processing ------------------------------------------------------------
offset <- read_excel(logFile, sheet = 'TIMEOFFSET') %>% filter(ID == routeID) # import Picarro G2401 time offset
picarro_v2 <- create_level_2(picarro_v1, offset$G2401, v2_file_path) # Level 2 data gets exported
print("Level 2 data has been created and exported")

# Level 3 Processing  ---------------------------------------------
picarro_gps_weather <- merge_picarro_datalogger(picarro_v2, paste(main, surveyInfo$GPSFILE,sep=""), paste(main,surveyInfo$WEATHERFILE,sep="")) %>% # merge Level 2 data with gps and weather
  select(-c(CH4, CO2)) %>% rename(CH4 = CH4_dry, CO2 = CO2_dry) # from here on, only use the CO2_dry and CH4_dry columns, they are renamed to CO2 and CH4

# CALIBRATE ---------------------------------------------------------------
calInfo <- read_excel(logFile, sheet = 'CALESTIMATES') %>% filter(ID == routeID) # load the calibration information

# get average measured values
# if don't have external file for calibrations, create one before continuing
source('C:/Users/cindy/limeavocado/create_cal_summary.R')
if (sum(is.na(calInfo$EXTFILE)) > 0) { 
  test <- create_cal_summary(calInfo, picarro_v2)
}

measured <- get_calib_values(calInfo)
sdevs <- filter(measured, VALUE == "STD") %>% select(-VALUE) # get standard deviations
measured <- filter(measured, VALUE == "Mean") %>% select(-VALUE) # get average measurements

# load known standard values
standards <- read_excel(logFile, sheet = 'STANDARDS') %>% filter(STANDARD_ID %in% calInfo$STANDARD_ID) 
standards <- standards[order(standards$STANDARD_ID),]

# calculate coefficients for correction
cal <- calibrate(measured, standards)
  
# apply the calibration for each gas
picarro_gps_weather <- mutate(picarro_gps_weather, 
                              CH4_corr = CH4*cal["CH4","slope"] + cal["CH4","intercept"],
                              CO2_corr = CO2*cal["CO2","slope"] + cal["CO2","intercept"],
                              CO_corr = CO*cal["CO","slope"] + cal["CO","intercept"])
# Exclusions --------------------------------------------------------------
# flag CO values (2019 data) 
picarro_gps_weather <- flagCO(picarro_gps_weather, start = picarro_v1$TIMESTAMP[1])
# make CO = NA if any CO values were flagged by flagCO() function
picarro_gps_weather$CO_corr[!is.na(picarro_gps_weather$CO_flag)] <- NA

# only include driving times (exclude calibrations) 
driveStart <- surveyInfo$STARTUTC
driveEnd <- surveyInfo$ENDUTC
picarro_gps_weather <- subset(picarro_gps_weather, TIMESTAMP >= driveStart & TIMESTAMP <= driveEnd)

# exclude anomalous data 
picarro_v3 <- mutate(picarro_gps_weather, 
                     Flag = case_when(CO_corr >= 100000   ~ "S_co", # CO anomaly
                                      CO2_corr >= 3000  ~ "S_co2", # CO2 anomaly
                                      CH4_corr >= 20 ~ "S_ch4", # CH4 anomaly
                                      CO_corr < 0 ~ "Neg_co",
                                      CO2_corr <0 ~ "Neg_co2",
                                      CH4_corr <0 ~ "Neg_ch4",
                                      is.na(CO2_corr) ~ "NA_co2",
                                      # is.na(CO_corr) ~ "NA_co",
                                      is.na(CH4_corr) ~ "NA_ch4")) 

picarro_v3 <- subset(picarro_v3, is.na(Flag)) # get only values that do not qualify as anomalous

# Save corrected data  ----------------------------------------------------
picarro_v3 <- picarro_v3[, -c(2,3,5,20,21)] #delete uncalibrated gas values & flag column
picarro_v3[,c(2:16)] <- lapply(picarro_v3[,c(2:16)], as.numeric)
picarro_v3 <- picarro_v3[, lapply(.SD, round, 4), TIMESTAMP] #round all numeric columns to 4 decimal places
picarro_v3 <- rename(picarro_v3, CH4 = CH4_corr, CO2 = CO2_corr, CO = CO_corr)

#Make a csv file of level 3 version of exported picarro data;
write.csv(picarro_v3, file=paste(v3_file_path, "picarro-G2401-",routeID,"_v3.csv", sep=""), row.names = FALSE)
print("Calibrated data has been saved as a csv.")

# Quality Control Report --------------------------------------------------
{qc <- list("Route ID" = routeID,
           "Files found" = length(files),
           "Level 1 length" = nrow(picarro_v1),
           "Level 2 length" = nrow(picarro_v2),
           "Level 3 length" = nrow(picarro_v3),
           "Total L1 flags" = sum(!is.na(picarro_v1$Flag)),
           "P Flags" = sum(picarro_v1$Flag == "P", na.rm = TRUE),
           "T Flags" = sum(picarro_v1$Flag == "T", na.rm = TRUE),
           "W Flags" = sum(picarro_v1$Flag == "W", na.rm = TRUE),
           "C Flags" = sum(picarro_v1$Flag == "C", na.rm = TRUE),
           "CO Flags" = sum(!is.na(picarro_gps_weather$CO_flag)),
           "Missing values" = sum(is.na(picarro_v3)),
           "Negative CO2 values" = sum(picarro_gps_weather$CO2 < 0, na.rm = TRUE),
           "Negative CO values" = sum(picarro_gps_weather$CO < 0, na.rm = TRUE),
           "Negative CH4 values" = sum(picarro_gps_weather$CH4 < 0, na.rm = TRUE),
           "CO2 > 3000 ppm" = sum(picarro_gps_weather$CO2 > 3000, na.rm = TRUE),
           "CO > 100000 ppb" = sum(picarro_gps_weather$CO > 100000, na.rm = TRUE),
           "CH4 > 20 ppm" = sum(picarro_gps_weather$CH4 > 20, na.rm = TRUE),
           "Minimum CO2 (ppm)" = min(picarro_v3$CO2, na.rm = TRUE),
           "Minimum CO (ppb)" = min(picarro_v3$CO, na.rm = TRUE),
           "Minimum CH4 (ppm)" = min(picarro_v3$CH4, na.rm = TRUE),
           "Maximum CO2 (ppm)" = max(picarro_v3$CO2, na.rm = TRUE),
           "Maximum CO (ppb)" = max(picarro_v3$CO, na.rm = TRUE),
           "Maximum CH4 (ppm)" = max(picarro_v3$CH4, na.rm = TRUE),
           "CO2 cal type" = cal["CO2","points"],
           "CO cal type" = cal["CO","points"],
           "CH4 cal type" = cal["CH4","points"],
           "Standard 1" = standards$STANDARD_ID[1],
           "CO2 standard 1" = standards$CO2[1],
           "CO2 measured 1" = round(measured$CO2[1],4),
           "CO2 std 1" = round(sdevs$CO2[1],4),
           "CO standard 1" = standards$CO[1],
           "CO measured 1" = round(measured$CO[1],4),
           "CO std 1" = round(sdevs$CO[1],4),
           "CH4 standard 1" = standards$CH4[1],
           "CH4 measured 1" = round(measured$CH4[1],4),
           "CH4 std 1" = round(sdevs$CH4[1],4),
           "Standard 2" = standards$STANDARD_ID[2],
           "CO2 standard 2" = standards$CO2[2],
           "CO2 measured 2" = round(measured$CO2[2],4),
           "CO2 std 2" = round(sdevs$CO2[2],4),
           "CO standard 2" = standards$CO[2],
           "CO measured 2" = round(measured$CO[2],4),
           "CO std 2" = round(sdevs$CO[2],4),
           "CH4 standard 2" = standards$CH4[2],
           "CH4 measured 2" = round(measured$CH4[2],4),
           "CH4 std 2" = round(sdevs$CH4[2],4),
           "CO2 intercept" = round(cal["CO2","intercept"], 6),
           "CO intercept" = round(cal["CO","intercept"], 6),
           "CH4 intercept" = round(cal["CH4","intercept"], 6),
           "CO2 slope" = round(cal["CO2","slope"], 6),
           "CO slope" = round(cal["CO","slope"], 6),
           "CH4 slope" = round(cal["CH4","slope"],6))
} # quality control calculations

qc <- as.matrix(unlist(qc)) # convert results to matrix
print(qc)
# save qaqc report
write.fwf(qc, file = paste(qaqc_file_path, 'qaqc_picarro-G2401_', routeID, '.txt', sep =""), rownames = TRUE, colnames = FALSE)

# plot the standard vs measured values
par(mfrow=c(1,3))
plot(standards$CO2, measured$CO2, pch = 19, cex = 1.5)
plot(standards$CO, measured$CO, pch = 19, cex = 1.5)
plot(standards$CH4, measured$CH4, pch = 19, cex = 1.5)

# # plot timeseries for each gas
par(mfrow=c(1,1))
plot(picarro_v3$TIMESTAMP, picarro_v3$CO2)
plot(picarro_v3$TIMESTAMP, picarro_v3$CO)
plot(picarro_v3$TIMESTAMP, picarro_v3$CH4)

# produce a report (run the following code in the console)
# rmarkdown::render(input = "~/limeavocado/limeavocado_picarro_G2401.R",
#                   output_format = "pdf_document",
#                   output_file = paste(report_file_path,"picarro_G2401_processing_report_",routeID,".pdf", sep=""))

