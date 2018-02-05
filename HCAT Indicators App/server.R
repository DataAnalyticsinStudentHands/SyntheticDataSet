library(shiny)
library(leaflet)

HCAT_prime=readRDS("HCAT_prime.RDS")
HCAT_real=readRDS("HCAT_real.RDS")


# Define server logic required to draw maps
shinyServer(function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  output$HCATprime <- renderLeaflet({
    pal <- colorNumeric(
      palette = "YlGnBu",
      domain = c(0,100)
    )
    
    popup=paste0("GEOID:",HCAT_prime@data$GEOID)
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = HCAT_prime, 
                  fillColor = ~pal(unlist(HCAT_prime@data[input$Variable])), 
                  color = "#b2aeae", # you need to use hex colors
                  fillOpacity = 0.7, 
                  weight = 1, 
                  smoothFactor = 0.2,
                  popup=popup)%>%
      addLegend(pal = pal, 
                values = unlist(HCAT_prime@data[input$Variable]), 
                position = "bottomright",
                labFormat = labelFormat(suffix="%"),
                title = paste("Houston Prime:",input$Variable))
  })
  
  output$HCATreal <- renderLeaflet({
    pal <- colorNumeric(
      palette = "YlGnBu",
      domain = c(0,100)
    )
    popupR=paste0("GEOID:",HCAT_real@data$GEOID)
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = HCAT_real, 
                  fillColor = ~pal(unlist(HCAT_real@data[input$Variable])), 
                  color = "#b2aeae", # you need to use hex colors
                  fillOpacity = 0.7, 
                  weight = 1, 
                  smoothFactor = 0.2,
                  popup=popupR)%>%
      addLegend(pal = pal, 
                values = unlist(HCAT_real@data[input$Variable]), 
                position = "bottomright",
                labFormat = labelFormat(suffix="%"),
                title = paste("Houston Real:",input$Variable))
  })
  output$Table<-renderTable({
    options=intersect(colnames(HCAT_prime@data),colnames(HCAT_real@data))
    options=options[8:length(options)]
    
    HCAT_Real_Indicators=HCAT_prime@data[c("GEOID",options)]
    colnames(HCAT_Real_Indicators)=c("GEOID",paste0("HCAT_REAL:",options))
    HCAT_Prime_Indicators=HCAT_prime@data[c("GEOID",options)]
    colnames(HCAT_Prime_Indicators)=c("GEOID",paste0("HCAT_PRIME:",options))
    
    Ranks=merge(HCAT_Real_Indicators,HCAT_Prime_Indicators,by="GEOID")
    unique(Ranks)
  })
})