flagCO <- function(data, start) {

  # for all 2019 surveys, flag the first hour of CO data
  if (surveyInfo$YEAR == 2019) {
    # start = data$TIMESTAMP[1] # get time of first measurement
    data <- mutate(data, CO_flag = case_when(TIMESTAMP <= start + 3600 ~ "first"))
  }
  
  # don't use CO data from 20190715, calibrations were bad
  if (routeID == "20190715") {
    data <- mutate(data, CO_flag = "all")
  }
  
}