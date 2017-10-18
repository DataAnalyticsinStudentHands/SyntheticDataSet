library(shiny)

#Load maps of tracts
Harris_tract_data <- readRDS("../maps/Harris_tract_data.rds")
#Load data
realfrequencypertract=read.csv("../maps/realfrequenciespertract.csv")
syntheticfrequencypertract=read.csv("../maps/syntheticdatasetfrequenciespertract.csv")

#to add right length location field to Harris_tract_data
Harris_tract_data$Location10 <- paste0(Harris_tract_data$STATEFP,Harris_tract_data$COUNTYFP,Harris_tract_data$TRACTCE)
realfrequencypertract$Location10 <- paste0(realfrequencypertract$state,realfrequencypertract$county,realfrequencypertract$tract)
library(tigris)
Houston_real <- geo_join(Harris_tract_data, realfrequencypertract, 'Location10', 'Location10', by = NULL, how = "left")
syntheticfrequencypertract$TRACTCE <- syntheticfrequencypertract$tract
Houston_prime <- geo_join(Harris_tract_data,syntheticfrequencypertract, 'TRACTCE', 'TRACTCE', by = NULL, how = "left")

library(leaflet)

# Define server logic required to draw maps
shinyServer(function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  output$houstonprime <- renderLeaflet({
    pal <- colorNumeric(
      palette = "YlGnBu",
      domain = union(Houston_prime@data[input$Variable],Houston_real@data[input$Variable])
    )
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = Houston_prime, 
                  fillColor = ~pal(Houston_prime@data[input$Variable]), 
                  color = "#b2aeae", # you need to use hex colors
                  fillOpacity = 0.7, 
                  weight = 1, 
                  smoothFactor = 0.2)%>%
      addLegend(pal = pal, 
                values = unlist(Houston_prime@data[input$Variable]), 
                position = "bottomright", 
                title = paste("Houston Prime:",input$Variable))
  })
  
  output$houstonreal <- renderLeaflet({
    pal <- colorNumeric(
      palette = "YlGnBu",
      domain = union(Houston_prime@data[input$Variable],Houston_real@data[input$Variable])
    )
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = Houston_real, 
                  fillColor = ~pal(Houston_real@data[input$Variable]), 
                  color = "#b2aeae", # you need to use hex colors
                  fillOpacity = 0.7, 
                  weight = 1, 
                  smoothFactor = 0.2)%>%
      addLegend(pal = pal, 
                values = unlist(Houston_real@data[input$Variable]), 
                position = "bottomright", 
                title = paste("Houston Real:",input$Variable))
  })
})