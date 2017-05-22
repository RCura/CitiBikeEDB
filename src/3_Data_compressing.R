library(tidyverse)
library(lubridate)

data_zip <- "data/citibike_data.zip"

stations <- read_csv(unz(data_zip, filename = "data/minified/CitiBike_stations.csv"))

library(rio)
batch_data <- import_list(data_zip)

citibike_data <-  bind_rows(batch_data[-13])

# On nettoie les stations
stations <- stations %>%
  filter(Long < -60) %>%
  filter(Lat > 40.65) %>%
  group_by(ID) %>%
  summarise(
    Name = last(Name),
    Lat = last(Lat),
    Long = last(Long)
  )


citibike_data <- citibike_data %>%
  filter(startID %in% stations$ID,
         endID %in% stations$ID,
         tripDuration >= 120,
         tripDuration <= 3600)



citibike_data <- citibike_data %>%
  mutate(startTime = ymd_hms(startTime),
         endTime = ymd_hms(endTime))


citibike_data <- citibike_data %>%
  select(-endTime) %>%
  rename(Time = startTime) %>%
  mutate(Date = lubridate::date(Time)) %>%
  mutate(Month = lubridate::month(Date, label = TRUE)) %>%
  mutate(WDay = lubridate::wday(Date, label = TRUE)) %>%
  mutate(WDay = factor(WDay,levels(WDay)[c(2:7, 1)])) %>%
  mutate(Day = lubridate::day(Date)) %>%
  mutate(DHour = lubridate::hour(Time) + lubridate::minute(Time)/60) %>%
  mutate(Hour = lubridate::hour(Time)) %>%
  mutate(userGender = factor(userGender, levels = c(0,1,2),labels = c("Unknown", "Male", "Female")))
  



save(citibike_data, stations, file = "data/citibike_data.RData", compress = TRUE)

