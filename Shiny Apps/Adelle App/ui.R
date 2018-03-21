library(shiny)
library(leaflet)

ui <- fluidPage(
  titlePanel("Adelle's App"),
  
  
  fluidRow(
    column(6,
           h4("Map of Adults Without Insurance"),
           leafletOutput("Map_Adults_Without_Insurance")
           ),
    column(6,
           h4("Map of Adults Without Routine Checkup"),
           leafletOutput("Map_Adults_Without_Checkup")
    )
  )
  
  

)
