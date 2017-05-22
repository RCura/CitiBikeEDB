library(shiny)
library(scales)
library(tidyverse)
library(leaflet)
library(leaflet.extras)

if (!exists("citibike_data")) {
  load("data/citibike_data.RData")
}


stations_start_mean <- citibike_data %>%
  group_by(startID) %>%
  summarise(NbStart = n())

stations_end_mean <- citibike_data %>%
  group_by(endID) %>%
  summarise(NbEnd = n())

stations_both <- stations %>%
  left_join(stations_start_mean, by = c("ID" = "startID")) %>%
  left_join(stations_end_mean, by = c("ID" = "endID"))

rm(stations_start_mean, stations_end_mean)
