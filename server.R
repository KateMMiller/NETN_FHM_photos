library(shiny)
library(leaflet)
library(htmltools)
library(dplyr)
library(shinyjs)
library(htmltools)

shiny_server <- function(session, input, output){
  
  output$plot_df <- renderUI({
    plots2 <- plots %>% filter(Unit_Code %in% input$park) %>% droplevels()
    selectizeInput(inputId = 'plot', 
                   label = h5("Zoom to a plot"), 
                   choices = c("Choose a plot" = "", unique(plots$Plot_Name)))
  })
  
  
  # Make NPS map Attribution

  NPSbasic = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck58pyquo009v01p99xebegr9/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  
  NPSimagery = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck72fwp2642dv07o7tbqinvz4/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  
  NPSslate = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpvc2e0avf01p9zaw4co8o/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  
  NPSlight = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpia2u0auf01p9vbugvcpv/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"

  
  output$forestMap <- renderLeaflet({
    leaflet() %>% 
      setView(lng = -71.6665, lat = 42.5, zoom = 6) %>% 
      setMaxBounds(lng1 = -67,
                   lng2 = -75,
                   lat1 = 46,
                   lat2 = 39) %>%
      addTiles(
        group = "Map",
        urlTemplate = NPSbasic) %>%
      addTiles(
        group = "Imagery",
        urlTemplate = NPSimagery) %>%
      addTiles(
        group = "Light",
        urlTemplate = NPSlight,
        options = tileOptions(minZoom = 8)
      ) %>%
      addTiles(
        group = "Slate",
        urlTemplate = NPSslate,
        options = tileOptions(minZoom = 8)
      ) %>%
      addLayersControl(
        map = .,
        baseGroups = c("Map", "Imagery", "Light", "Slate"),
        options = layersControlOptions(collapsed = T)
      ) 

  })
  
  # Zoom to a plot
  observe({
    req(input$forestMap_zoom)
    
    leafletProxy("forestMap") %>% 
        addCircleMarkers(
          data = plots,
          radius = 4,
          lng = plots$Longitude,
          lat = plots$Latitude,
          layerId = plots$Plot_Name, 
          label = if(input$forestMap_zoom > 14) substr(plots$Plot_Name, 6, 9) else NULL,
          labelOptions = labelOptions(noHide = TRUE, 
                                      textOnly = TRUE, 
                                      direction = "bottom", 
                                      textsize = "11px"),
          fillColor = "ForestGreen",
          fillOpacity = 0.75,
          weight = 1,
          color = "DimGrey"
    )
    
  })
  
  # Set up ability to zoom to given plot
  observeEvent(input$plotZoom, {
    req(input$plot)
    
    plot_selected <- plots %>% filter(Plot_Name == input$plot)
    
    output$UR<-renderText({c('<p> Click on a point in the map to view photopoints </p>')})
    output$BR<-renderText({c('<p> </p>')})
    output$BL<-renderText({c('<p> </p>')})
    output$UL<-renderText({c('<p> </p>')})
    
    leafletProxy('forestMap') %>% 
      clearControls() %>%
      #clearPopups() %>% 
      setView(
        lng =  plot_selected$Longitude, 
        lat = plot_selected$Latitude, 
        zoom = 16) 
    delay(400, leafletProxy("forestMap") %>% 
            addCircles(
              lng = plot_selected$Longitude,
              lat = plot_selected$Latitude,
              layerId = substr(plot_selected$Plot_Name, 6, 9),
              group = 'pulse',
              radius = 19,
              color = '#00ffff',
              fillOpacity = 0,
              weight = 5)) 
    delay(1000, 
          leafletProxy('forestMap') %>% 
            clearShapes())
  })
  
  # Reset view of map panel
  observeEvent(input$reset_view, {
    reset("plotZoom")

    updateSelectizeInput(session, 'park',
                         choices = c("Choose a park" = "",
                                     "ACAD", "MABI", "MIMA", "MORR", "ROVA", "SAGA", "SARA", "WEFA"))

    updateSelectizeInput(session, 'plot',
                         choices = c("Choose a plot" = "", unique(plots$Plot_Name)))
    
    output$UR <- renderText({c('<p> Click on a point in the map to view photopoints </p>')})
    output$BR <- renderText({c('<p> </p>')})
    output$BL <- renderText({c('<p> </p>')})
    output$UL <- renderText({c('<p> </p>')})
    
    leafletProxy("forestMap") %>% 
      clearPopups() %>%
      clearControls() %>% 
      setView(lng = -71.6665, lat = 42.5, zoom = 6) 
    

  })
  
  
  observeEvent(input$park, {
   req(input$park)
   
   park_coords <- plots %>% filter(Unit_Code %in% input$park) %>% summarize(long = mean(Longitude),
                                                                            lat = mean(Latitude))  
   
   park_plots <- plots %>% filter(Unit_Code %in% input$park) %>% select(Plot_Name) %>% unique()
   

   zoom_level <- ifelse(input$park == "ACAD", 9.5, 12.5)
   
   updateSelectizeInput(session, 'plot',
                        choices = c("Choose a plot" = "", park_plots$Plot_Name))
   
  leafletProxy('forestMap') %>% 
     #clearControls() %>% 
     clearPopups() %>% 
     setView(lng = park_coords$long,
             lat = park_coords$lat, 
             zoom = zoom_level) 
  })
  
  # Set up ability to zoom to given plot
  observeEvent(input$plot, {
    req(input$plot)
    
    plot_selected <- plots %>% filter(Plot_Name %in% input$plot) 
    
    photoUR <- as.character(plot_selected %>% 
                             mutate(photoUR = paste0(UR)) %>%  
                             select(photoUR) %>% droplevels())
    
    output$UR <- renderText({c('<img src="', photoUR,'" width="99%"/>')})
    
    photoBR <- as.character(plot_selected %>%  
                             mutate(photoBR = paste0(BR)) %>%  
                             select(photoBR) %>% droplevels())
    
    output$BR <- renderText({c('<img src="', photoBR,'" width="99%"/>')})
    
    photoBL <- as.character(plot_selected %>% 
                             mutate(photoBL = paste0(BL)) %>%  
                             select(photoBL) %>% droplevels())
    
    output$BL <- renderText({c('<img src="', photoBL,'" width="99%"/>')})
    
    photoUL <- as.character(plot_selected %>% 
                             mutate(photoUL = paste0(UL)) %>%  
                             select(photoUL) %>% droplevels())
    
    output$UL <- renderText({c('<img src="', photoUL,'" width="99%"/>')})
    
    leafletProxy('forestMap') %>% 
      #clearControls() %>% 
      #clearPopups() %>% 
      setView(
        lng =  plot_selected$Longitude,
        lat = plot_selected$Latitude,
        zoom = 14)

  })
    
  observeEvent(input$forestMap_marker_click, {
      
      MarkerClick <- input$forestMap_marker_click
      plot_click <- plots[plots$Plot_Name == MarkerClick$id, ]

      plots_park <- plot_click %>% filter(Unit_Code %in% input$park) %>% unique()
      
      tempdata <- plot_click %>% select(Plot_Name, Longitude, Latitude, Panel, 
                                        Num_Live_Trees, Num_Dead_Trees, Inv_Shrub_Cover, 
                                        Num_Seedlings, Num_Saplings, Num_Species, Location_Notes)

      content <- paste0("<div class='leaflet-popup-scrolled' style='max-width:600px;max-height:250px;'>",
                        "<b>", h4("Plot Info:"), "</b>",
                        tagList(tags$table(
                          class = 'table',
                          tags$thead(tags$th("Metric"), tags$th("Values")),
                          tags$tbody(
                            mapply(FUN = function(Name, Value){
                              tags$tr(tags$td(sprintf("%s: ", Name)),
                                      tags$td(align = 'right', sprintf("%s", Value)))},
                              Name = names(tempdata[,c(1, 4:11)]),
                              Value = tempdata[,c(1, 4:11)],
                              SIMPLIFY = FALSE)))), "</div>")

      
      updateSelectizeInput(session, 'park',
        choices = c("Choose a park" = "",
                    "ACAD", "MABI", "MIMA", "MORR", "ROVA", "SAGA", "SARA", "WEFA"),
        selected = paste(plot_click$Unit_Code)
        )
      
      updateSelectizeInput(session, 'plot',
                           choices = c("Choose a plot" = "", unique(plots_park$Plot_Name)),
                           selected = paste(plot_click$Plot_Name))
      
      photoUR<- as.character(plot_click %>% filter(Plot_Name == MarkerClick$id) %>% 
                             mutate(photoUR = paste0(UR)) %>%  
                             select(photoUR) %>% droplevels())
      
      output$UR <- renderText({c('<img src="', photoUR,'" width="99%"/>')})
      
      photoBR<- as.character(plot_click %>% filter(Plot_Name == MarkerClick$id) %>% 
                               mutate(photoBR = paste0(BR)) %>%  
                               select(photoBR) %>% droplevels())
      
      output$BR <- renderText({c('<img src="', photoBR,'" width="99%"/>')})
      
      photoBL<- as.character(plot_click %>% filter(Plot_Name == MarkerClick$id) %>% 
                               mutate(photoBL = paste0(BL)) %>%  
                               select(photoBL) %>% droplevels())
      
      output$BL <- renderText({c('<img src="', photoBL,'" width="99%""/>')})
      
      photoUL<- as.character(plot_click %>% filter(Plot_Name == MarkerClick$id) %>% 
                               mutate(photoUL = paste0(UL)) %>%  
                               select(photoUL) %>% droplevels())
      
      output$UL <- renderText({c('<img src="', photoUL,'" width="99%"/>')})
 
      leafletProxy('forestMap') %>% 
        setView(
          lng =  tempdata$Longitude,
          lat = tempdata$Latitude,
          zoom = 14) %>% 
        addPopups(
          lng = tempdata$Longitude,
          lat = tempdata$Latitude,
          popup = content
        ) 
    })
    
   


}
