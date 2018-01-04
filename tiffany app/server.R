# Define server logic required to draw a histogram ----

library(leaflet)

server <- function(input, output) {
  
  output$Percent_Gas_Stoves_Map<-renderLeaflet({
    Houston_Gas_Stoves=readRDS("Houston_Gas_Stoves.RDS")
    popup <- paste0("GEOID: ", Houston_Gas_Stoves$GEOID, "<br>", 
                    "% Gas Stoves ", 
                    Houston_Gas_Stoves$Lower.Confidence.Interval, "<",
                    Houston_Gas_Stoves$X..HOME.FURNISHINGS...APPLIANCES...KITCHEN.APPLIANCES...KITCHEN.APPL..MAJOR.APPL..ITEMS.OWNED...GAS.STOVE.OR.RANGE..2015, ">",
                    Houston_Gas_Stoves$Upper.Confidence.Interval)
    pal <- colorNumeric(
      palette = "YlGnBu",
      domain = Houston_Gas_Stoves$X..HOME.FURNISHINGS...APPLIANCES...KITCHEN.APPLIANCES...KITCHEN.APPL..MAJOR.APPL..ITEMS.OWNED...GAS.STOVE.OR.RANGE..2015
    )
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = Houston_Gas_Stoves, 
                  fillColor = ~pal(X..HOME.FURNISHINGS...APPLIANCES...KITCHEN.APPLIANCES...KITCHEN.APPL..MAJOR.APPL..ITEMS.OWNED...GAS.STOVE.OR.RANGE..2015), 
                  color = "#b2aeae", # you need to use hex colors
                  fillOpacity = 0.7, 
                  weight = 1, 
                  smoothFactor = 0.2,
                  popup = popup) %>%
      addLegend(pal = pal, 
                values = Houston_Gas_Stoves$X..HOME.FURNISHINGS...APPLIANCES...KITCHEN.APPLIANCES...KITCHEN.APPL..MAJOR.APPL..ITEMS.OWNED...GAS.STOVE.OR.RANGE..2015, 
                position = "bottomright", 
                title = "Percent of<br>Gas Stoves",
                labFormat = labelFormat(suffix = "%"))
  })
  output$Number_Adults_Meeting_Threshold<-renderLeaflet({
    Houston_Adults_Threshold<-readRDS("Houston_Adults_Threshold.RDS")
    popup2 <- paste0("GEOID: ", Houston_Adults_Threshold$GEOID, "<br>", 
                    "Adults with Diabetes ")
    
    pal2 <- colorNumeric(
      palette = "YlGnBu",
      domain = Houston_Adults_Threshold$peopleatthreshold
      
    )
    
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = Houston_Adults_Threshold, 
                  fillColor = ~pal2(peopleatthreshold), 
                  color = "#b2aeae", # you need to use hex colors
                  fillOpacity = 0.7, 
                  weight = 1, 
                  smoothFactor = 0.2,
                  popup = popup2) %>%
      addLegend(pal = pal2, 
                values = Houston_Adults_Threshold$peopleatthreshold, 
                position = "bottomright", 
                title = "Number Meeting<br>Higher Exposure<br>Levels")

  })
  
  output$People_with_diabetes<-renderLeaflet({
    
    Houston_Diabetes=readRDS("Houston_Diabetes.RDS")
    popup <- paste0("GEOID: ", Houston_Diabetes$GEOID, "<br>", 
                    "Adults with Diabetes ")
    
    pal <- colorNumeric(
      palette = "YlGnBu",
      domain = Houston_Diabetes$X..Diabetes..2015
      
    )
leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = Houston_Diabetes, 
                  fillColor = ~pal(Houston_Diabetes$X..Diabetes..2015), 
                  color = "#b2aeae", # you need to use hex colors
                  fillOpacity = 0.7, 
                  weight = 1, 
                  smoothFactor = 0.2,
                  popup = popup) %>%
      addLegend(pal = pal, 
                values = Houston_Diabetes$X..Diabetes..2015, 
                position = "bottomright", 
                title = "Number of adults with Diabetes")
  })
   
}