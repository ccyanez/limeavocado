# data was downloaded from CalTrans PeMS Clearinghouse
# https://pems.dot.ca.gov/?dnode=Clearinghouse&type=station_hour&district_id=7&submit=Submit
library(dplyr)
library(readxl)

# load data
year <- '2021'
main <- '/Volumes/cindrive/COVID/Data/PeMS/'
df <- read.csv(paste(main, 'd07_text_station_hour_',year,'_07.txt', sep = ''), header = FALSE)

# select only essential columns
# ignored lane-specific values (only looked station-wide)
df <- select(df, c(1:12))

# format columns
colnames(df) <- c('timestamp',
                  'station',
                  'district',
                  'route',
                  'direction',
                  'lanetype',
                  'station_length',
                  'samples',
                  'observed',
                  'flow', 
                  'occupancy',
                  'speed')

# filter only mainline (ML) freeway stations
df <- filter(df, lanetype == "ML")

# filter only "good" data (% obs > 90)
df <- filter(df, observed > 90)

# read in survey dates from metadata file 
dates <- read_excel('~/Documents/COVID_AirQuality/Data/LIMEAVOCADO/level03/metadata.xlsx', sheet = 'dates') %>% 
  select(paste("LA",year,sep="")) %>% na.omit()
colnames(dates) <- "date"
dates$date <- as.Date(dates$date)

# filter only survey dates
df$date <- as.Date(df$timestamp, format = "%m/%d/%Y")
df <- filter(df, date %in% dates$date)

# load coordinates for each station
meta <- read.delim(paste(main,'d07_text_meta_2021_10_20.txt',sep="")) %>%
  select("ID","Latitude","Longitude","Lanes")
colnames(meta) <- c("station", "latitude", "longitude", "lanes")

# merge dataframe with station coordinates 
df <- merge(df, meta, by = "station" )

# save output as new csv file
write.csv(df, file = paste(main, 'pems_reduced_',year,'.csv', sep=''), row.names = FALSE)
