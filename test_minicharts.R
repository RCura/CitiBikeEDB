library(tidyverse)
library(leaflet)
library(leaflet.minicharts)

leaflet(stations_both) %>%
  addProviderTiles(providers$Stamen.TonerLite,
                   options = providerTileOptions(opacity = 0.3)) %>%
  addMinicharts(lng = stations_both$Long,
                lat = stations_both$Lat,
                chartdata = stations_both %>% select(NbEnd, NbStart),
                type = "polar-area",
                 maxValues = stations_both %>% select(NbStart, NbEnd) %>% max(., na.rm = TRUE),
                colorPalette = c("#FB6C60", "#00B1FB")
                ) %>%
  # addCircles(lng = ~Long,
  #            lat = ~Lat,
  #            radius = ~rescale(sqrt(NbStart), to = c(1, 200)),
  #            stroke = TRUE,
  #            color = "white",
  #            weight = 1,
  #            fillColor = "red",
  #            fillOpacity = 1) %>%
  addDrawToolbar(polylineOptions = FALSE, polygonOptions = FALSE, circleOptions = FALSE,
                 markerOptions = FALSE, singleFeature = TRUE,
                 editOptions = editToolbarOptions(edit = FALSE, remove = TRUE))
