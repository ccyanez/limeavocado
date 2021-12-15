# use this function to get calibration measured averages and sdev
# saves a calibration summary file 
# only run if: is.na(calInfo$EXTFILE) == TRUE

create_cal_summary <- function(calInfo, picarro_v2) {
  
  # convert start/end times to datetime format
  calInfo$T1 <- as.POSIXct(paste(calInfo$DATE, calInfo$T1), tz ='America/Los_Angeles')
  calInfo$T2 <- as.POSIXct(paste(calInfo$DATE, calInfo$T2), tz ='America/Los_Angeles')
  
  # get unique tank IDs
  tanks <- unique(calInfo$STANDARD_ID)
  
  for (i in 1:length(tanks)) {
    # subset estimated calibration times for tank
    tankcals <- subset(calInfo, STANDARD_ID == tanks[i])
    
    # subset and bind tank runs
    cal <- picarro_v2[FALSE, ] # initalize empty subsetted dataframe
    for (c in 1:nrow(tankcals)) {
      temp <- picarro_v2 %>% filter( between(TIMESTAMP, tankcals$T1[c], tankcals$T2[c]) ) # subset estimated times
      plot(temp$TIMESTAMP, temp$CO2_dry) # plot subsetted times 
      rows <- identify(temp$TIMESTAMP, temp$CO2_dry) # interactively select actual start and end times 
      act_times <- temp[rows, "TIMESTAMP"]
      
      # subset the actual times
      sub <- picarro_v2 %>% filter( between(TIMESTAMP, act_times[1], act_times[2]))
      
      # bind with the other runs from this tank
      cal <- rbind(cal, sub)
    }
    
    # save information about this tank's calibrations
    summary <- sapply(cal, function(cal) c("STD" = sd(cal),
                                           "Mean" = mean(cal, na.rm = TRUE)))
    summary <- as.data.frame(summary)
    
    output_file <- paste(calSum_file_path, 'picarro-G2401-', routeID, '_', tanks[i],'_calibration.csv', sep="")
    write.csv(summary, file = output_file)
    print("Calibration summary has been saved as a csv file")
    
    calInfo$EXTFILE <- output_file

  }
  
  return(calInfo)
}