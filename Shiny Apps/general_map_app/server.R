library(shiny)
library(leaflet)

#These next 17 lines really only need to be run once so they will and just have the resulting spacial frame read in using RDS
#special code for Carol can't figure out how to install tigris on the server
#Load maps of tracts
#Harris_tract_data <- readRDS("Harris_tract_data.rds")
#Load data
#realfrequencypertract=read.csv("realfrequenciespertract.csv")
#syntheticfrequencypertract=read.csv("syntheticdatasetfrequenciespertract.csv")

#to add right length location field to Harris_tract_data
#Harris_tract_data$Location10 <- paste0(Harris_tract_data$STATEFP,Harris_tract_data$COUNTYFP,Harris_tract_data$TRACTCE)
#realfrequencypertract$Location10 <- paste0(realfrequencypertract$state,realfrequencypertract$county,realfrequencypertract$tract)
#library(tigris)
#Houston_real <- geo_join(Harris_tract_data, realfrequencypertract, 'Location10', 'Location10', by = NULL, how = "left")
#syntheticfrequencypertract$TRACTCE <- syntheticfrequencypertract$tract
#Houston_prime <- geo_join(Harris_tract_data,syntheticfrequencypertract, 'TRACTCE', 'TRACTCE', by = NULL, how = "left")
#saveRDS(Houston_prime,"Houston_prime.RDS")
#saveRDS(Houston_real,"Houston_real.RDS")
Houston_prime=readRDS("Houston_prime.RDS")
Houston_real=readRDS("Houston_real.RDS")


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
      domain = union(unlist(Houston_prime@data[input$Variable]),unlist(Houston_real@data[input$Variable]))
    )
    
    popup=paste0("GEOID:",Houston_prime@data$GEOID,"<br>",
                 "number of households:",Houston_prime@data$Households,"<br>",
                 "number of simulated households:",Houston_prime@data$Simulated_Households,"<br>",
                 "number of people in the Census:",Houston_prime@data$total_people_recorded_in_Census,"<br>",
                 "simulated number of people:",Houston_prime$number_of_simulated_people)
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = Houston_prime, 
                  fillColor = ~pal(unlist(Houston_prime@data[input$Variable])), 
                  color = "#b2aeae", # you need to use hex colors
                  fillOpacity = 0.7, 
                  weight = 1, 
                  smoothFactor = 0.2,
                  popup=popup)%>%
      addLegend(pal = pal, 
                values = unlist(Houston_prime@data[input$Variable]), 
                position = "bottomright", 
                title = paste("Houston Prime:",input$Variable))
  })
  
  output$houstonreal <- renderLeaflet({
    pal <- colorNumeric(
      palette = "YlGnBu",
      domain = union(unlist(Houston_prime@data[input$Variable]),unlist(Houston_real@data[input$Variable]))
    )
    popupR=paste0("GEOID:",Houston_real@data$GEOID,"<br>",
                 "number of households:",Houston_real@data$Households,"<br>",
                 "number of simulated households:",Houston_real@data$Simulated_Households,"<br>",
                 "number of people in the Census:",Houston_real@data$total_people_recorded_in_Census,"<br>",
                 "simulated number of people:",Houston_real$number_of_simulated_people)
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = Houston_real, 
                  fillColor = ~pal(unlist(Houston_real@data[input$Variable])), 
                  color = "#b2aeae", # you need to use hex colors
                  fillOpacity = 0.7, 
                  weight = 1, 
                  smoothFactor = 0.2,
                  popup=popupR)%>%
      addLegend(pal = pal, 
                values = unlist(Houston_real@data[input$Variable]), 
                position = "bottomright", 
                title = paste("Houston Real:",input$Variable))
  })
})