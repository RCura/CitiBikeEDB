library(tidyverse)
library(lubridate)

csvFiles <- list.files(path = "data/", pattern = "*tripdata.csv")

citibike_columns <- c("tripDuration","startTime", "endTime",
                      "startID", "startName", "startLat", "startLong",
                      "endID", "endName", "endLat", "endLong",
                      "bikeID",  "userType", "userBirth", "userGender")

for (i in 1:12){
  thisFile <- csvFiles[i]
  
  citibike_data <- read_csv(thisFile,
                            col_names = citibike_columns,
                            col_types = c("iccccddccddccic"),
                            skip = 1) %>%
    filter(tripDuration <= 3600) # on enleve les déplacements > 1h (moins de 2%)
  if (i < 10){
    citibike_data <- citibike_data %>%
      mutate(startTime = mdy_hms(startTime), # For files 01 -> 09
             endTime = mdy_hms(endTime))# For files 01 -> 09
  } else {
    citibike_data <- citibike_data %>%
      mutate(startTime = ymd_hms(startTime), # For files 10 -> 12
             endTime = ymd_hms(endTime))# For files 10 -> 12
  }
   
  
  citibike_stations <- citibike_data %>%
    select(starts_with("start"), starts_with("end"), -ends_with("Time"))
  
  citibike_start <-  citibike_data %>%
    select(starts_with("start"), -ends_with("Time")) %>%
    rename(ID = startID,
           Name = startName,
           Lat = startLat,
           Long = startLong) %>%
    group_by(ID, Name, Lat, Long) %>%
    summarise()
  
  citibike_end <- citibike_data %>%
    select(starts_with("end"), -ends_with("Time")) %>%
    rename(ID = endID,
           Name = endName,
           Lat = endLat,
           Long = endLong) %>%
    group_by(ID, Name, Lat, Long) %>%
    summarise()
  
  citibike_stations <- citibike_start %>%
    bind_rows(citibike_end) %>%
    group_by(ID, Name, Lat, Long) %>%
    summarise()
  
  citibike_data <- citibike_data %>%
    select(-startLat, -startLong, -startName, -endLat, -endLong, -endName)
  
  newName <- gsub(thisFile, pattern = ".csv", replacement = "_mini.csv")
  newPathName <- paste0("data/minified/", newName)
  write_csv(citibike_data, newPathName)
  
  stationName <- gsub(thisFile, pattern = "tripdata.csv", replacement = "stations.csv")
  stationPath <- paste0("data/minified/", stationName)
  write_csv(citibike_stations, stationPath)
}


