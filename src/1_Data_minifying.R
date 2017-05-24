library(tidyverse)
library(lubridate)

csvFiles <- list.files(path = "data/", pattern = "*tripdata.csv", full.names = TRUE)

citibike_columns <- c("tripDuration","startTime", "endTime",
                      "startID", "startName", "startLat", "startLong",
                      "endID", "endName", "endLat", "endLong",
                      "bikeID",  "userType", "userBirth", "userGender")

citibike_stations <- data_frame(ID = character(), Name = character(), Lat = double(), Long = double())

for (i in 1:12){
  thisFile <- csvFiles[i]
  
  citibike_data <- read_csv(thisFile,
                            col_names = citibike_columns,
                            col_types = c("iccccddccddccic"),
                            skip = 1)
  if (i < 10){
    citibike_data <- citibike_data %>%
      mutate(startTime = mdy_hms(startTime), # For files 01 -> 09
             endTime = mdy_hms(endTime))# For files 01 -> 09
  } else {
    citibike_data <- citibike_data %>%
      mutate(startTime = ymd_hms(startTime), # For files 10 -> 12
             endTime = ymd_hms(endTime))# For files 10 -> 12
  }
   
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
  
  citibike_both <- citibike_start %>%
    bind_rows(citibike_end) %>%
    group_by(ID, Name, Lat, Long) %>%
    summarise()
  
  citibike_stations <- citibike_stations %>%
    bind_rows(citibike_both) %>%
    group_by(ID) %>%
    summarise(
      Name = last(Name),
      Lat = last(Lat),
      Long = last(Long)
    )
  
  citibike_data <- citibike_data %>%
    select(-startLat, -startLong, -startName, -endLat, -endLong, -endName)
  
  newName <- gsub(thisFile, pattern = ".csv", replacement = "_mini.csv")
  write_csv(citibike_data, newName)
  file.remove(thisFile)
  
  write_csv(citibike_stations, "data/CitiBike-Stations.csv", append = FALSE)
}


