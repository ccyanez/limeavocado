### future:
# subset_calibration(picarro_gps_weather)
# calibrate(picarro_gps_weather)
# subset_driving_time(picarro_gps_weather)
# create_level_3(picarro_gps_weather)
# time_average(picarro_gps_weather)
# regrid(picarro_gps_weather)


get_files <- function(surveyInfo, in_path) {
  #############################################################################################
  # function mission: find hourly Picarro files for the user-specified survey
  #     The function binds hourly Picarro files into one big file, creates a TIMESTAMP column, and converts CH4 units to ppm
  #     It also prints how many files it found. If "0 files found" is printed, the file paths may be entered incorrectly
  # inputs: 
  #       (1) surveyInfo = the "general" sheet from the logs.xlsx which includes information about survey dates
  #       (2) in_path = the path to the folder where the raw Picarro data is stored in your computer. Once in this folder, files
  #             should be stored in folders organized by date YYYY > MM > DD
  # outputs: 
  #       (1) picarro_v1 = level 1 version of data 
  #############################################################################################
  # get route details
    yr <- surveyInfo$YEAR
    mon <- surveyInfo$MONTH
    day <- surveyInfo$DAY
    leak <- surveyInfo$LEAK

    picarro_files <- list.files(paste(in_path,yr,"/",mon,"/",day,sep=""), pattern = "*CFKADS2306-", full.names = TRUE) # find files based on pattern
    # if data runs into the next day ("leak") because of the time zone difference, import the next day's files too
    # picarro records files in UTC time (which is 7 to 8 hours ahead of PST (depending on daylight savings)
    if (leak == "yes") {
      # only works if the next day is in the same month
      nextDay <- list.files(paste(in_path,yr,"/",mon,"/",as.numeric(day)+1,sep=""), pattern = "*CFKADS", full.names = TRUE)
      # nextDay <- list.files(paste(path,yr,"/","08","/","01",sep=""), pattern = "*CFKADS2306-", full.names = TRUE) # temporary solution if next day is in next month
      picarro_files <- c(picarro_files,nextDay)
    }
    print(paste(length(picarro_files), "files found")) # print how many files were found, should be 1 file per hour of data
    
    return(picarro_files)
}

create_level_1 <- function(files, out_path) {
  #############################################################################################
  # function mission: create level 1 version of Picarro G2401 data from list of file paths
  #     The function binds hourly Picarro files into one big file, creates a TIMESTAMP column, and converts CH4 units to ppm
  # inputs: 
  #       (1) list of filepaths to the picarro files of interest
  #       (2) the output path where you want to save the level 1 data
  # outputs: 
  #       (1) picarro_v1 = level 1 version of data 
  #############################################################################################
  picarro_v1 <- rbindlist(lapply(files,fread, sep=" ", fill=T)) # bind the raw data files into one big file = LEVEL 1 DATA
  picarro_v1$TIMESTAMP <- as.POSIXct(paste(picarro_v1$DATE, picarro_v1$TIME), tz="UTC", format="%Y-%m-%d %H:%M:%S") # combines DATE and TIME columns
  picarro_v1$CH4 <- picarro_v1$CH4/1000 # unit conversions for methane
  picarro_v1$CH4_dry <- picarro_v1$CH4_dry/1000 # unit conversions for methane
  write.csv(picarro_v1, file=paste(out_path,"picarro-G2401-",routeID,"_v1.csv", sep="")) # Export level 1 file
  
  return(picarro_v1)
}

create_level_2 <- function(picarro_v1, offset_seconds, out_path) {
  #############################################################################################
  # function mission: create level 2 version of Picarro G2401 data from level 1 data
  #     The function selects only columns with trace gas species and corrects the instrument time offset
  # inputs: 
  #     (1) picarro_v1 = level 1 version of picarro G2401 data
  #     (2) offset_seconds = time offset of the picarro G2401 from the actual time (in seconds)
  #     (3) out_path = the file path where you want to export the level 2 data
  # output: 
  #     picarro_v2 = level 2 version of picarro data 
  #############################################################################################
  
  picarro_v2 <- select(picarro_v1, TIMESTAMP, CH4, CH4_dry, CO2, CO2_dry, H2O, CO) # select only columns of interest in the order you want them
  picarro_v2 <- picarro_v2[, lapply(.SD, round, 4), TIMESTAMP]   # Round all numeric columns to 4 decimal places
  picarro_v2$TIMESTAMP <- picarro_v2$TIMESTAMP - seconds(offset_seconds) # correct time offset
  write.csv(picarro_v2, file=paste(out_path, "picarro-G2401-",routeID,"_v2.csv", sep="")) # Export level 2 file
   
  return(picarro_v2)
}

merge_picarro_datalogger <- function(picarro_v2, gpsfile, weatherfile) {
  #############################################################################################
  # function mission: merge level 2 picarro data with gps and weather data from datalogger
  # inputs: 
  #     (1) picarro_v2 = level 2 version of picarro G2401 data
  #     (2) gpsfile = the file path to the gps data for this survey
  #     (3) weatherfile = the file path to the weather data for this survey
  # output: 
  #     picarro_gps_weather = merged version of level 2 picarro data with gps and weather data
  #############################################################################################
  # read GPS and weather data 
  gps <- read.csv(gpsfile, header = T, row.names = 1)
  weather <- read.csv(weatherfile, header = T, row.names = 1)
  # get timestamp column into datetime format
  gps$TIMESTAMP <- as.POSIXct(paste(gps$TIMESTAMP), tz="UTC", format="%Y-%m-%d %H:%M:%S")
  weather$TIMESTAMP <- as.POSIXct(paste(weather$TIMESTAMP), tz="UTC", format="%Y-%m-%d %H:%M:%S")
  # merge gps, picarro, and weather data
  picarro_gps = merge(picarro_v2, gps, all=FALSE) #merges picarro and gps by TIMESTAMP (UTC)
  picarro_gps_weather = merge(picarro_gps, weather, all=FALSE) #merges picarro, gps, and weather by TIMESTAMP (UTC)
  
  return(picarro_gps_weather)
  
}

## UNDER CONSTRUCTION!!!
subset_calib_manually <- function(data, t1_est, t2_est) {
  #############################################################################################
  # function mission: interactively subsets the times when you were calibrating. For use when you have a range of times but don't know 
  #                     EXACTLY when the standard measurements started and ended. The function will plot the range of times you give it
  #                     and you can interactively choose the start and end times by clicking on the plot. You can then save these values
  #                     into your logs.xlsx file so you don't need to do it manually next time.
  # inputs: 
  #     (1) data = a dataframe with the values you are going to calibrate (picarro_gps_weather)
  #     (2) t1_est = estimated start times
  #     (3) t2_est = estimated end times
  # output: 
  #     (1) sub = subsetted data for chosen times 
  #     (2) t1 = actual calibration start
  #     (3) t2 = actual calibration end
  #############################################################################################
  # if TIME_EST = TRUE, will make you choose start/end times interactively
  
  # create plot of this time range to select final calibration start and end
  temp <- subset(picarro_v2, TIMESTAMP >= t1 & TIMESTAMP <= t2)
  print("Select the beginning and end of the calibration on the plot, then click Finish")
  plot(temp$TIMESTAMP, temp$CO2_dry)
  rows <- identify(temp$TIMESTAMP, temp$CO2_dry)
  act_calib_times <- temp[rows,TIMESTAMP]
  # subset the data
  sub <- subset(picarro_v2, TIMESTAMP >= act_calib_times[1] & TIMESTAMP <= act_calib_times[2])
  
  # save actual calibration times to logs.xlsx file
  # https://stackoverflow.com/questions/32632137/using-write-xlsx-in-r-how-to-write-in-a-specific-row-or-column-in-excel-file
  # library(xlsx)
  # # load file contents
  # file <- "yourfilename.xlsx"
  # wb <- loadWorkbook(file)
  # sheets <- getSheets(wb)
  # sheet <- sheets[[1]]  # or another
  # # data to put into B10:H20 range
  # data <- matrix(runif(7*11), nrow=11, ncol=7)
  # # modify contents
  # addDataFrame(data, sheet, col.names = FALSE, row.names = FALSE,
  #              startRow = 10, startColumn = 2)
  # # save to disk
  # saveWorkbook(wb, file)
  
  # if TIME_EST = FALSE, will use the exact values
  sub <- subset(picarro_v2, TIMESTAMP >= t1 & TIMESTAMP <= t2)

  return(sub)
}

# UNDER CONSTRUCTION
# calibrate <- function(picarro_gps_weather, calib_details)
# {
#   # subset first calibration ("AM")
#   if (strftime(calib_details$UTC1START, format="%H:%M:%S") != "00:00:00") {
#     sub1 <- subset_calib(calib_details$UTC1START, calib_details$UTC1END)
#   }
#   
#   # subset second calibration ("PM")
#   if (strftime(calib_details$UTC2START, format="%H:%M:%S") != "00:00:00") {
#     sub2 <- subset_calib(calib_details$UTC2START, calib_details$UTC2END)
#   }
#   
# }
