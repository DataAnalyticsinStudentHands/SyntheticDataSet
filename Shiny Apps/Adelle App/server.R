# Define server logic required to draw a histogram ----
library(shiny)
library(leaflet)
adults=readRDS("data_for_map.rds")

server <- function(input, output) {
  popup <- paste0("GEOID:",adults$Location,"<br>",
                  "% Adults without insurance",adults$adults_without_insurance,"<br>",
                  "% Adults without regular check ups",adults$adults_without_routine_checkups)
  
  pal_Insurance <- colorNumeric(
    palette = "YlGnBu",
    domain = adults$adults_without_insurance
  )
  
  pal_Checkup <- colorNumeric(
    palette = "YlGnBu",
    domain = adults$adults_without_routine_checkups
  )
  output$Map_Adults_Without_Insurance=renderLeaflet(
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = adults, 
                  fillColor = ~pal_Insurance(adults$adults_without_insurance), 
                  color = "#b2aeae", # you need to use hex colors
                  fillOpacity = 0.7, 
                  weight = 1, 
                  smoothFactor = 0.2,
                  popup = popup) %>%
      addLegend(pal = pal_Insurance, 
                values = adults$adults_without_insurance, 
                position = "bottomright", 
                title = "Percent of<br>Adults without Insurance",
                labFormat = labelFormat(suffix = "%"))
  )
  output$Map_Adults_Without_Checkup=renderLeaflet(
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = adults, 
                  fillColor = ~pal_Checkup(adults$adults_without_routine_checkups), 
                  color = "#b2aeae", # you need to use hex colors
                  fillOpacity = 0.7, 
                  weight = 1, 
                  smoothFactor = 0.2,
                  popup = popup) %>%
      addLegend(pal = pal_Checkup, 
                values = adults$adults_without_routine_checkups, 
                position = "bottomright", 
                title = "Percent of<br>Adults without Routine Checkups",
                labFormat = labelFormat(suffix = "%"))
  )
  
   
}