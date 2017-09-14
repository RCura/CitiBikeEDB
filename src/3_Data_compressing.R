library(tidyverse)
library(lubridate)

stations <- read_csv("data/CitiBike-Stations.csv", col_types = "ccdd")

citibike_data <- dir( path = "data/", pattern = "*_mini.csv", full.names = TRUE) %>%
  map_df(read_csv, col_types = "iTTcccccc")

# On nettoie les stations

stations <- stations %>%
  filter(Long < -60) %>% # On enleve les stations trop lointaines
  filter(Lat > 40.65) # Idem


# On nettoie les trajets
citibike_data <- citibike_data %>%
  filter(startID %in% stations$ID,
         endID %in% stations$ID) %>%
  filter(tripDuration <= 3600) %>% # on enleve les déplacements > 1h (moins de 2%)
  filter(tripDuration >= 120) # on enleve les déplacements < 2 min

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
  
save(citibike_data, stations, file = "data/CitiBike_Data.RData", compress = TRUE)

