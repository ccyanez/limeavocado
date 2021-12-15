# use this function to calibrate surveys
# will be designed to support various scenarios
# after running this function, you will have averaged calibration values

get_calib_values <- function(calInfo) {
	# initialize output table
	measured <- data.frame(STANDARD_ID = factor(), TYPE = factor(), CH4 = double(), CO2 = double(), CO = double())

	for (i in 1:nrow(calInfo)) { 
		
		# if have an external file ("calibration summary")
		if (!is.na(calInfo$EXTFILE[i])) {
			summary <- read.csv(calInfo$EXTFILE[i], header = T, row.names = 1)
			# summary <- summary["Mean", ] # gets just the row with the averages
			summary <- cbind(calInfo$STANDARD_ID[i], calInfo$TYPE[i], summary) # bind with tank ID and type (high or low)
			summary <- select(summary, c(1,2), CO2_dry, CO, CH4_dry) %>% rownames_to_column()
			names(summary) <- c("VALUE","STANDARD_ID","TYPE","CO2","CO","CH4")
			
			measured <- rbind(measured, summary)
			}
		
		# if do not have an external file: (you will create one for future use)
		else {
		  et1 <- as.POSIXct(paste(calInfo$DATE, calInfo$T1), tz ='America/Los_Angeles') # get estimated start time
		  et2 <- as.POSIXct(paste(calInfo$DATE, calInfo$T2), tz ='America/Los_Angeles') # get estimated end time
		  
		  temp <- subset(picarro_v2, TIMESTAMP >= t1 & TIMESTAMP <= t2) # subset level 2 data with the estimated times
		  plot(temp$TIMESTAMP, temp$CO2_dry) # create subsetted plot for tha time range
		  rows <- identify(temp$TIMESTAMP, temp$CO2_dry) # identify actual start and end times interactively
		  act_times <- temp[rows, "TIMESTAMP"] # save the real calibration times 
		  # future: save actual times somewhere in your computer too 
		  
		  # subset the data with actual times
		  sub <- subset(picarro_v2, TIMESTAMP >= act_times[1] & TIMESTAMP <= act_times[2])
		  
		  # if (exists("high1")) {
		  #   # high <- rbind(high1, high2)
		  #   high <- high1
		  #   high_summary <- sapply(high, function(high) c( "STD" = sd(high),
		  #                                                  "Mean" = mean(high, na.rm = TRUE)))
		  #   
		  #   high_summary <- as.data.frame(high_summary)
		  #   print("Summary of high calibration subset: ")
		  #   print(high_summary)
		  #   write.csv(high_summary, file=paste("~/UCR LIME AVOCADO/Data/processed/calibration_summary/picarro-G2401-",yr,mon,day,"_high_calibration.csv",sep=""))
		  #   print("This summary has been saved as a .csv file")
		  # }
		  
			# print("you need to subset your calibration")
			# summary <- "coming soon"
			
			# measured <- rbind(measured, summary)
		}
	  
	}
	
		measured <- distinct(measured) # get only unique calibration runs
		measured <- measured[order(measured$STANDARD_ID),] # sort by tank ID 
		return(measured)
}