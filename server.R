library(shiny)

shinyServer(function(session, input, output) {
  
  output$map <- renderLeaflet({
    leaflet(stations_both) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(opacity = 0.3)) %>%
      addCircles(lng = ~Long,
                 lat = ~Lat,
                 radius = ~rescale(sqrt(NbStart), to = c(1, 200)),
                 stroke = TRUE,
                 color = "white",
                 weight = 1,
                 fillColor = "red",
                 fillOpacity = 1) %>%
      addDrawToolbar(polylineOptions = FALSE, polygonOptions = FALSE, circleOptions = FALSE,
                     markerOptions = FALSE, singleFeature = TRUE,
                     editOptions = editToolbarOptions(edit = FALSE, remove = TRUE))
  })
  
  filtredSpatialData <- reactive({
    if (length(input$map_draw_all_features$features) > 0) {
      coordsSelectBox <- unlist(input$map_draw_all_features$features[[1]]$geometry$coordinates)[c(1,2,4,5)]
      filtredStations <- stations %>%
        filter(Long >= coordsSelectBox[1], Long <= coordsSelectBox[4]) %>%
        filter(Lat >= coordsSelectBox[2], Lat <= coordsSelectBox[3])
      
      tmpFiltredSpatialData <- citibike_data %>%
        filter(startID %in% filtredStations$ID)
      
      return(tmpFiltredSpatialData)
    }
  })
 
  
  observe({# Update Map on attributes selections

    if (length(filtredTemporalData()) > 1) {
      map_data <- filtredTemporalData() %>%
        group_by(startID) %>%
        summarise(NbStart = n()) %>%
        left_join(stations, by = c("startID" = "ID"))
      
      mapProxy <- leafletProxy("map", session = session, data = map_data)
      mapProxy %>%
        clearShapes() %>%
        addCircles(lng = ~Long,
                   lat = ~Lat,
                   radius = ~rescale(sqrt(NbStart), to = c(1, 200)),
                   stroke = TRUE,
                   color = "white",
                   weight = 1,
                   fillColor = "red",
                   fillOpacity = 1)
      
    } else {
      mapProxy <- leafletProxy("map", session = session, data = stations_both)
      mapProxy %>%
        clearShapes() %>%
        addCircles(lng = ~Long,
                   lat = ~Lat,
                   radius = ~rescale(sqrt(NbStart), to = c(1, 200)),
                   stroke = TRUE,
                   color = "white",
                   weight = 1,
                   fillColor = "red",
                   fillOpacity = 1)
        
    }
  })
  
  
  filtredTemporalData <- reactive({# Filter on time/attribute data
    noSelection <- TRUE
    currentlyFiltred <- citibike_data
    
    if (!is.null(input$hourPlot_brush)) {
      thisSelection <- input$hourPlot_brush
      currentlyFiltred <- currentlyFiltred %>%
        filter(DHour >= thisSelection$xmin, DHour <= thisSelection$xmax)
      noSelection <- FALSE
    }
    
    if (!is.null(input$dayPlot_brush)) {
      thisSelection <- input$dayPlot_brush
      currentlyFiltred <- currentlyFiltred %>%
        filter(as.numeric(WDay) >= round(thisSelection$xmin),
               as.numeric(WDay) <= round(thisSelection$xmax))
      noSelection <- FALSE
    }
    
    if (!is.null(input$monthPlot_brush)) {
      thisSelection <- input$monthPlot_brush
      currentlyFiltred <- currentlyFiltred %>%
        filter(as.numeric(Month) >= round(thisSelection$xmin),
               as.numeric(Month) <= round(thisSelection$xmax))
      noSelection <- FALSE
    }
    
    if (!is.null(input$durationPlot_brush)) {
      thisSelection <- input$durationPlot_brush
      currentlyFiltred <- currentlyFiltred %>%
        filter(round(tripDuration / 60) >= thisSelection$xmin,
               round(tripDuration / 60) <= thisSelection$xmax)
      noSelection <- FALSE
    }
    
    if (!is.null(input$agePlot_brush)) {
      thisSelection <- input$agePlot_brush
      currentlyFiltred <- currentlyFiltred %>%
        filter((2016 - userBirth) >= min(c(thisSelection$xmin, 0)),
               (2016 - userBirth) <= max(c(thisSelection$xmax, 120)))
      noSelection <- FALSE
    }
    
    if (!is.null(input$sexPlot_brush)) {
      thisSelection <- input$sexPlot_brush
      currentlyFiltred <- currentlyFiltred %>%
        filter(as.numeric(userGender) >= round(thisSelection$xmin),
               as.numeric(userGender) <= round(thisSelection$xmax))
      noSelection <- FALSE
    }
    
    if (!noSelection) {
      return(currentlyFiltred)
    }
  })
  
  output$hourPlot <- renderPlot({
    hourData <- citibike_data %>%
      group_by(DHour) %>%
      summarise(Nb = n()) %>%
      mutate(Freq = Nb / sum(Nb))
    
    hourPlot <- ggplot(hourData, aes(DHour, Freq)) +
      stat_smooth(se = FALSE, span = 0.1, geom = "area", fill = "#43a2ca", col = "#053144", alpha = .35, method = "loess", na.rm = TRUE) +
      scale_x_continuous(limits = c(0,24)) +
      scale_y_continuous(limits = c(0, NA)) +
      theme_minimal()
    
    if (length(filtredSpatialData()) > 1) {
      selData <- filtredSpatialData() %>%
        group_by(DHour) %>%
        summarise(Nb = n()) %>%
        mutate(Freq = Nb / sum(Nb))
      
      hourPlot <- hourPlot +
        stat_smooth(data = selData, aes(DHour, Freq),
          se = FALSE, span = 0.1, geom = "area", fill = "#67000d", col = "red", alpha = .35, method = "loess", na.rm = TRUE)
    }
    
    if (input$showTempFiltred && length(filtredTemporalData()) > 1) {
      selData <- filtredTemporalData() %>%
        group_by(DHour) %>%
        summarise(Nb = n()) %>%
        mutate(Freq = Nb / sum(Nb))
      
      hourPlot <- hourPlot +
        stat_smooth(data = selData, aes(DHour, Freq),
                    se = FALSE, span = 0.1, geom = "area", fill = "#D7C76F", col = "#847215", alpha = .35, method = "loess", na.rm = TRUE)
    }
      return(hourPlot)
  })
  
  output$dayPlot <- renderPlot({
    dayData <- citibike_data %>%
      group_by(WDay) %>%
      summarise(Nb = n()) %>%
      mutate(Freq = Nb / sum(Nb))
    
    dayPlot <- ggplot(dayData, aes(WDay, Freq)) +
      geom_col(fill = "#43a2ca", alpha = .35, col = "#053144") + 
      theme_minimal()
    
    if (length(filtredSpatialData()) > 1) {
      selData <- filtredSpatialData() %>%
        group_by(WDay) %>%
        summarise(Nb = n()) %>%
        mutate(Freq = Nb / sum(Nb))
      
      dayPlot <- dayPlot +
        geom_col(data = selData, aes(WDay, Freq), fill = "#67000d", col = "red", alpha = .35)
    }
    
    if (input$showTempFiltred && length(filtredTemporalData()) > 1) {
      selData <- filtredTemporalData() %>%
        group_by(WDay) %>%
        summarise(Nb = n()) %>%
        mutate(Freq = Nb / sum(Nb))
      
      dayPlot <- dayPlot +
        geom_col(data = selData, aes(WDay, Freq), fill = "#D7C76F", col = "#847215", alpha = .35)
    }
    return(dayPlot)
  })
  
  output$monthPlot <- renderPlot({
    monthData <- citibike_data %>%
      group_by(Month) %>%
      summarise(Nb = n()) %>%
      mutate(Freq = Nb / sum(Nb))
    
    monthPlot <- ggplot(monthData, aes(Month, Freq)) +
      geom_col(fill = "#43a2ca", alpha = .35, col = "#053144") + 
      theme_minimal()
    
    if (length(filtredSpatialData()) > 1) {
      selData <- filtredSpatialData() %>%
        group_by(Month) %>%
        summarise(Nb = n()) %>%
        mutate(Freq = Nb / sum(Nb))
      
      monthPlot <- monthPlot +
        geom_col(data = selData, aes(Month, Freq), fill = "#67000d",col = "red", alpha = .35)
    }
    
    if (input$showTempFiltred && length(filtredTemporalData()) > 1) {
      selData <- filtredTemporalData() %>%
        group_by(Month) %>%
        summarise(Nb = n()) %>%
        mutate(Freq = Nb / sum(Nb))
      
      monthPlot <- monthPlot +
        geom_col(data = selData, aes(Month, Freq), fill = "#D7C76F",col = "#847215", alpha = .35)
    }
    return(monthPlot)
    
  })
    
  output$durationPlot <- renderPlot({
    durationData <- citibike_data %>%
      group_by(duration = round(tripDuration / 60)) %>%
      summarise(Nb = n()) %>%
      mutate(Freq = Nb / sum(Nb))
    
    durationPlot <- ggplot(durationData, aes(duration, Freq)) +
      stat_smooth(se = FALSE, span = .5, geom = "area", fill = "#43a2ca", alpha = .35, method = "loess", col = "#053144", na.rm=TRUE) +
      scale_y_continuous(limits = c(0, NA)) +
      scale_x_continuous(limits = c(2, 40)) +
      theme_minimal()
    
    if (length(filtredSpatialData()) > 1) {
      selData <- filtredSpatialData() %>%
        group_by(duration = round(tripDuration / 60)) %>%
        summarise(Nb = n()) %>%
        mutate(Freq = Nb / sum(Nb))
      
      durationPlot <- durationPlot +
        stat_smooth(data = selData, aes(duration, Freq),
                    se = FALSE, span = .5, geom = "area", fill = "#67000d",col = "red", alpha = .35, method = "loess", na.rm=TRUE)
    }
    
    if (input$showTempFiltred && length(filtredTemporalData()) > 1) {
      selData <- filtredTemporalData() %>%
        group_by(duration = round(tripDuration / 60)) %>%
        summarise(Nb = n()) %>%
        mutate(Freq = Nb / sum(Nb))
      
      durationPlot <- durationPlot +
        stat_smooth(data = selData, aes(duration, Freq),
                    se = FALSE, span = .5, geom = "area", fill = "#D7C76F",col = "#847215", alpha = .35, method = "loess", na.rm=TRUE)
    }
    return(durationPlot)
  })
  
  output$agePlot <- renderPlot({
    ageData <- citibike_data %>%
      group_by(Age = (2016 - userBirth)) %>%
      summarise(Nb = n()) %>%
      mutate(Freq = Nb / sum(Nb)) %>%
      filter(Age <= 80)
    
    agePlot <- ggplot(ageData, aes(Age, Freq)) +
      stat_smooth(se = FALSE, span = 1, geom = "area", fill = "#43a2ca", alpha = .35, method = "loess", col = "#053144", na.rm=TRUE) +
      scale_x_continuous(limits = c(16, 75)) +
      theme_minimal()
    
    if (length(filtredSpatialData()) > 1) {
      selData <- filtredSpatialData() %>%
        group_by(Age = (2016 - userBirth)) %>%
        summarise(Nb = n()) %>%
        mutate(Freq = Nb / sum(Nb)) %>%
        filter(Age <= 80)
      
      agePlot <- agePlot +
        stat_smooth(data = selData, aes(Age, Freq),
                    se = FALSE, span = 1, geom = "area", fill = "#67000d", col = "red",alpha = .35, method = "loess", na.rm=TRUE)
    }
    
    if (input$showTempFiltred && length(filtredTemporalData()) > 1) {
      selData <- filtredTemporalData() %>%
        group_by(Age = (2016 - userBirth)) %>%
        summarise(Nb = n()) %>%
        mutate(Freq = Nb / sum(Nb)) %>%
        filter(Age <= 80)
      
      agePlot <- agePlot +
        stat_smooth(data = selData, aes(Age, Freq),
                    se = FALSE, span = 1, geom = "area", fill = "#D7C76F", col = "#847215",alpha = .35, method = "loess", na.rm=TRUE)
    }
    return(agePlot)
  })
  
  output$sexPlot <- renderPlot({
    sexData <- citibike_data %>%
      group_by(userGender) %>%
      summarise(Nb = n()) %>%
      mutate(Freq = Nb / sum(Nb))
    
    sexPlot <- ggplot(sexData, aes(userGender, Freq)) +
      geom_col(fill = "#43a2ca", alpha = .35, col = "#053144") +
      theme_minimal()
    
    if (length(filtredSpatialData()) >  1) {
      selData <- filtredSpatialData() %>%
        group_by(userGender) %>%
        summarise(Nb = n()) %>%
        mutate(Freq = Nb / sum(Nb))
      
      sexPlot <- sexPlot +
        geom_col(data = selData, aes(userGender, Freq), fill = "#67000d", col = "red",alpha = .35)
    }
    
    if (input$showTempFiltred && length(filtredTemporalData()) >  1) {
      selData <- filtredTemporalData() %>%
        group_by(userGender) %>%
        summarise(Nb = n()) %>%
        mutate(Freq = Nb / sum(Nb))
      
      sexPlot <- sexPlot +
        geom_col(data = selData, aes(userGender, Freq), fill = "#D7C76F", col = "#847215",alpha = .35)
    }
    return(sexPlot)
  })
  
})

