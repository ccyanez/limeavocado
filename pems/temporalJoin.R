# before reaching this step...
# pems data has been reduced (pems_reduce.R)
# spatial join has been done between pems data and CO2xs data (ArcGIS)

library(dplyr)
library(lubridate)

# load data
year <- '2021'
main <- '/Volumes/cindrive/COVID/Data/PeMS/joined/'
df <- read.csv(paste(main, 'pems_CO2xs_',year,'.txt', sep = '')) %>%
  rename(timestamp = "timestamp_")

# convert both datasets to same time zone
df$timestamp <- as.POSIXct(df$timestamp, 
                           format = '%m/%d/%Y %H:%M:%S',
                           tz = "America/Los_Angeles")
df$TIMESTAMP1 <- as.POSIXct(df$TIMESTAMP1, 
                            format = '%m/%d/%Y %H:%M:%S',
                            tz = "UTC")
df$TIMESTAMP1 <- with_tz(df$TIMESTAMP1, tzone = "America/Los_Angeles")

# calculate the difference in time between the two timestamps
df <- mutate(df, timediff = timestamp - TIMESTAMP1)

# keep if the difference between the two timestamps is less than one hour
df <- filter(df, abs(timediff) <= 3600)

# keep the closest match for each station
match <- df %>% 
  group_by(station) %>% 
  mutate(match = ifelse(abs(timediff) == min(abs(timediff)), "best","0")) %>% 
  filter(match == "best")

# # test plot to see how the correlation looks
# plot(match$flow, match$CO2xs)
# plot(match$flow, match$CO)
# plot(match$CO, match$CO2xs)
# plot(match$timestamp, match$flow)
# fit <- lm(flow ~ CO2xs, data = match)
# summary(fit)
