# use this function to calibrate surveys
# will be designed to support various scenarios
# after running this function, you will have averaged calibration values

get_calib_values <- function(calInfo) {
	# initialize output table
	measured <- data.frame(STANDARD_ID = factor(), TYPE = factor(), CH4 = double(), CO2 = double(), CO = double())

	for (i in 1:nrow(calInfo)) { 
		
		# if have an external file (calibration summary)
		if (!is.na(calInfo$EXTFILE[i])) {
			summary <- read.csv(calInfo$EXTFILE[i], header = T, row.names = 1)
			summary <- summary["Mean", ]
			summary <- cbind(calInfo$STANDARD_ID[i], calInfo$TYPE[i], summary, row.names = NULL)
			summary <- select(summary, c(1,2), CO2_dry, CO, CH4_dry)
			names(summary) <- c("STANDARD_ID","TYPE","CO2","CO","CH4")
			
			measured <- rbind(measured, summary)
			}
		
		# if do not have an external file
		else {
			print("you need to subset your calibration")
			summary <- "coming soon"
			measured <- rbind(measured, summary)
		}
		}
		measured <- distinct(measured) # get only unique calibration runs
		measured <- measured[order(measured$STANDARD_ID),] # sort by tank ID 
		return(measured)
}