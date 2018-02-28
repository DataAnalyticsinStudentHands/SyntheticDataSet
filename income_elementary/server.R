library(shiny)
library(leaflet)


# Define server logic required to draw maps
shinyServer(function(input, output) {
  

  
  output$income_and_elementary_schools <- renderLeaflet({

    tract_map=readRDS("tract_map.RDS")
    elementary_school_zones_of_interest=readRDS("elementary_school_zones_of_interest.RDS")
    
    pal <- colorNumeric(
      palette = "RdYlBu",
      domain = c(0,400000)
    )
    
    popup=paste0("Elementary School:",elementary_school_zones_of_interest$Elementary)
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = tract_map, 
                  fillColor = ~pal(unlist(tract_map$Median.Household.Income..2017)), 
                  color = "#b2aeae", # you need to use hex colors
                  fillOpacity = 0.7, 
                  weight = 1,
                  group="Median Household Income",
                  smoothFactor = 0.2)%>%
      addPolygons(data = tract_map, 
                  fillColor = ~pal(unlist(tract_map$Average.Household.Income..2017)), 
                  color = "#b2aeae", # you need to use hex colors
                  fillOpacity = 0.7, 
                  weight = 1,
                  group="Average Household Income",
                  smoothFactor = 0.2)%>%
      addPolygons(data = tract_map, 
                  fillColor = ~pal(unlist(tract_map$Family.Income.Median..2017)), 
                  color = "#b2aeae", # you need to use hex colors
                  fillOpacity = 0.7, 
                  weight = 1,
                  group="Median Family Income",
                  smoothFactor = 0.2)%>%
      addPolygons(data = tract_map, 
                  fillColor = ~pal(unlist(tract_map$Average.Household.Income..2017)), 
                  color = "#b2aeae", # you need to use hex colors
                  fillOpacity = 0.7, 
                  weight = 1,
                  group="Average Family Income",
                  smoothFactor = 0.2)%>%
      addPolylines(data=elementary_school_zones_of_interest,
                   popup=popup,
                   weight=2,
                   color="black",
                   opacity=1,
                   group="School Zone"
      )%>%
      addLayersControl(
        baseGroups=c("Median Household Income","Average Household Income","Median Family Income","Average Family Income"),
        overlayGroups="School Zone",
        options=layersControlOptions(collapsed=FALSE)
      )%>%
      addLegend(pal = pal, 
                values = c(tract_map$Family.Income.Median..2017,tract_map$Family.Income.Average2017,tract_map$Average.Household.Income..2017,tract_map$Median.Household.Income..2017), 
                position = "bottomright", 
                title = paste("Income"))
    

  })
  

})