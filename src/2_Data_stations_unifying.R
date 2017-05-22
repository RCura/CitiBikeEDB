library(tidyverse)


stationFiles <- paste0("data/",list.files(path = "data/minified/",  pattern = "*stations*"))

stations_data <- read_csv(file = stationFiles[1])

for (thisFile in stationFiles[-1]) {
  thisData <- read_csv(thisFile)
  stations_data <- stations_data %>%
    bind_rows(thisData)
}

stations <- stations_data %>%
  group_by(ID) %>%
  summarise(Name = last(Name),
            Lat = last(Lat),
            Long = last(Long)) %>%
  mutate(ID = as.character(ID))
write_csv(stations, path = "data/minified/CitiBike_stations.csv")

zip("data/minified/", zipfile = "data/citibike_data.zip")
