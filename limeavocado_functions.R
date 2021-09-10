### future:
get_route_data(routeID)
create_level_1(picarro_raw)
correct_time_offset(picarro_v1)
create_level_2(picarro_v2)
merge_picarro_datalogger(picarro_v2, gps, weather)
subset_calibration(picarro_gps_weather)
calibrate(picarro_gps_weather)
subset_driving_time(picarro_gps_weather)
create_level_3(picarro_gps_weather)
time_average(picarro_gps_weather)
regrid(picarro_gps_weather)


get_files <- function(routeID)
{
  # get route details from logs.xlsx file
    gen <- read_excel("Data/logs.xlsx", sheet = 'GENERAL') %>% filter(ID == routeID)
    yr <- gen$YEAR
    mon <- gen$MONTH
    day <- gen$DAY
    leak <- gen$LEAK

    # find the files on your computer using the date (files should be in folders organized by date YYYY > MM > DD)
    path <- "Data/raw/Picarro G2401/"
    picarro_raw <- list.files(paste(path,yr,"/",mon,"/",day,sep=""), pattern = "*CFKADS2306-", full.names = TRUE)
    # if data runs into the next day ("leak") because of the time zone difference, import the next day's files too
    # picarro records files in UTC time (which is 7 to 8 hours ahead of PST (depending on daylight savings)
    if (leak == "yes") {
      # only works if the next day is in the same month
      nextDay <- list.files(paste(path,yr,"/",mon,"/",as.numeric(day)+1,sep=""), pattern = "*CFKADS", full.names = TRUE)
      # nextDay <- list.files(paste(path,yr,"/","08","/","01",sep=""), pattern = "*CFKADS2306-", full.names = TRUE) # temporary solution if next day is in next month
      picarro_raw <- c(picarro_raw,nextDay)
    }
    print(paste(length(picarro_raw), "files found")) # print how many files were found, should be 1 file per hour of data

    # bind the raw data files into one big file = LEVEL 1 DATA
    picarro_v1 <- rbindlist(lapply(picarro_raw,fread, sep=" ", fill=T)) #(level 1)
    #combines DATE and TIME columns
    picarro_v1$TIMESTAMP <- as.POSIXct(paste(picarro_v1$DATE, picarro_v1$TIME), tz="UTC", format="%Y-%m-%d %H:%M:%S")
    # unit conversions for methane
    picarro_v1$CH4 <- picarro_v1$CH4/1000
    picarro_v1$CH4_dry <- picarro_v1$CH4_dry/1000

    return(picarro_v1)
}

read_gps <- function(routeID, yy)
{
  gps_raw = read.delim(paste('Data/raw/datalogger/',yy,'/',routeID,'/CR1000X_GPS16X.dat',sep=""), sep=",", header = FALSE) #import GPS csv file
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

  return(gps_v1)
}

calibrate <- function(picarro_gps_weather, calib_details)
{
  # subset first calibration ("AM")
  if (strftime(calib_details$UTC1START, format="%H:%M:%S") != "00:00:00") {
    sub1 <- subset_calib(calib_details$UTC1START, calib_details$UTC1END)
  }

  # subset second calibration ("PM")
  if (strftime(calib_details$UTC2START, format="%H:%M:%S") != "00:00:00") {
    sub2 <- subset_calib(calib_details$UTC2START, calib_details$UTC2END)
  }

}

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
