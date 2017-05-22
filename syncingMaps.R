library(shiny)
library(tidyverse)
library(leaflet)
library(leaflet.extras)

ui <- fluidPage(
fluidRow(
  column(6, leafletOutput("start", height = "500px")),
  column(6, leafletOutput("end", height = "500px"))
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
output$start <- renderLeaflet({
  leaflet(stations_both) %>%
    addProviderTiles(providers$Stamen.TonerLite,
                     options = providerTileOptions(opacity = 0.3)) %>%
    addCircles(lng = ~Long,
               lat = ~Lat,
               radius = ~sqrt(NbStart) /2,
               stroke = TRUE,
               color = "white",
               weight = 1,
               fillColor = "red",
               fillOpacity = 1) %>%
    addDrawToolbar(polylineOptions = FALSE, polygonOptions = FALSE, circleOptions = FALSE,
                   markerOptions = FALSE, singleFeature = TRUE,
                   editOptions = editToolbarOptions(edit = FALSE, remove = TRUE))
})

output$end <- renderLeaflet({
  leaflet(stations_both) %>%
    addProviderTiles(providers$Stamen.TonerLite,
                     options = providerTileOptions(opacity = 0.3)) %>%
    addCircles(lng = ~Long,
               lat = ~Lat,
               radius = ~sqrt(NbEnd) /2,
               stroke = TRUE,
               color = "white",
               weight = 1,
               fillColor = "red",
               fillOpacity = 1) %>%
    addDrawToolbar(polylineOptions = FALSE, polygonOptions = FALSE, circleOptions = FALSE,
                   markerOptions = FALSE, singleFeature = TRUE,
                   editOptions = editToolbarOptions(edit = FALSE, remove = TRUE))
})

observe({ # Update startMap
  req(input$end_draw_all_features)
  if (length(input$end_draw_all_features$features) > 0) {
    coordsSelectBox <- unlist(input$end_draw_all_features$features[[1]]$geometry$coordinates)[c(1,2,4,5)]
    filtredStations <- stations %>%
      filter(Long >= coordsSelectBox[1], Long <= coordsSelectBox[4]) %>%
      filter(Lat >= coordsSelectBox[2], Lat <= coordsSelectBox[3])
    
    filtredDF <- citibike_data %>%
      filter(endID %in% filtredStations$ID) %>%
      group_by(startID, endID) %>%
      summarise(NbStart = n()) %>%
      group_by(startID) %>%
      summarise(NbStart = sum(NbStart, na.rm = TRUE))
    
    startStations <- stations %>%
      left_join(filtredDF, by = c("ID" = "startID")) %>%
      filter(NbStart > 0)
    
    startMapProxy <- leafletProxy("start", data = startStations)
    startMapProxy %>%
      clearShapes() %>%
      addCircles(lng = ~Long,
                 lat = ~Lat,
                 radius = ~sqrt(NbStart),
                 stroke = TRUE,
                 color = "white",
                 weight = 1,
                 fillColor = "red",
                 fillOpacity = 1)
  } else {
    startMapProxy <- leafletProxy("start", data = stations_both)
    startMapProxy %>%
      clearShapes() %>%
      addCircles(lng = ~Long,
                 lat = ~Lat,
                 radius = ~sqrt(NbStart) / 2,
                 stroke = TRUE,
                 color = "white",
                 weight = 1,
                 fillColor = "red",
                 fillOpacity = 1)
  }
})

observe({ # Update endMap
  req(input$start_draw_all_features)
  if (length(input$start_draw_all_features$features) > 0) {
    coordsSelectBox <- unlist(input$start_draw_all_features$features[[1]]$geometry$coordinates)[c(1,2,4,5)]
    filtredStations <- stations %>%
      filter(Long >= coordsSelectBox[1], Long <= coordsSelectBox[4]) %>%
      filter(Lat >= coordsSelectBox[2], Lat <= coordsSelectBox[3])
    
    filtredDF <- citibike_data %>%
      filter(startID %in% filtredStations$ID) %>%
      group_by(startID, endID) %>%
      summarise(NbEnd = n()) %>%
      group_by(endID) %>%
      summarise(NbEnd = sum(NbEnd, na.rm = TRUE))
    
    endStations <- stations %>%
      left_join(filtredDF, by = c("ID" = "endID")) %>%
      filter(NbEnd > 0)
    
    endMapProxy <- leafletProxy("end", data = endStations)
    endMapProxy %>%
      clearShapes() %>%
      addCircles(lng = ~Long,
                 lat = ~Lat,
                 radius = ~sqrt(NbEnd),
                 stroke = TRUE,
                 color = "white",
                 weight = 1,
                 fillColor = "red",
                 fillOpacity = 1)
  } else {
    endMapProxy <- leafletProxy("end", data = stations_both)
    endMapProxy %>%
      clearShapes() %>%
      addCircles(lng = ~Long,
                 lat = ~Lat,
                 radius = ~sqrt(NbEnd) / 2,
                 stroke = TRUE,
                 color = "white",
                 weight = 1,
                 fillColor = "red",
                 fillOpacity = 1)
  }
  
  
})



}

# Run the application 
shinyApp(ui = ui, server = server)

