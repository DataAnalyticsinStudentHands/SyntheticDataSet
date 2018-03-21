library(shiny)
library(leaflet)


# Define server logic required to draw maps
shinyServer(function(input, output) {
  

  
  output$income_and_elementary_schools <- renderLeaflet({

    tract_map=readRDS("tract_map.RDS")
    elementary_school_zones_of_interest=readRDS("elementary_school_zones_of_interest.RDS")
    
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = c(0,70)
    )
    
    popup=paste0("Elementary School:",elementary_school_zones_of_interest$Elementary)
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = tract_map, 
                  fillColor = ~pal(unlist(tract_map$Total.Households.with.Public.Assistance.Income.or.Food.Stamps.SNAP..2017)), 
                  color = "#b2aeae", # you need to use hex colors
                  fillOpacity = 0.7, 
                  weight = 1,
                  group="Percentage Households with Public Assistance Income or Food Stamps SNAP",
                  smoothFactor = 0.2)%>%
      addPolygons(data = tract_map, 
                  fillColor = ~pal(unlist(tract_map$Total.Households.with.Public.Assistance.Income..2017)), 
                  color = "#b2aeae", # you need to use hex colors
                  fillOpacity = 0.7, 
                  weight = 1,
                  group="Percentage Households with Public Assistance Income",
                  smoothFactor = 0.2)%>%
      addPolygons(data = tract_map, 
                  fillColor = ~pal(unlist(tract_map$Population.in.Poverty.2017)), 
                  color = "#b2aeae", # you need to use hex colors
                  fillOpacity = 0.7, 
                  weight = 1,
                  group="Percentage Population in Poverty",
                  smoothFactor = 0.2)%>%
      
      addPolylines(data=elementary_school_zones_of_interest,
                   popup=popup,
                   weight=2,
                   color="black",
                   opacity=1,
                   group="School Zone"
      )%>%
      addLayersControl(
        baseGroups=c("Percentage Households with Public Assistance Income or Food Stamps SNAP","Percentage Households with Public Assistance Income","Percentage Population in Poverty"),
        overlayGroups="School Zone",
        options=layersControlOptions(collapsed=FALSE)
      )%>%
      addLegend(pal = pal, 
                values = c(0:70), 
                position = "bottomright")
    

  })
  

})