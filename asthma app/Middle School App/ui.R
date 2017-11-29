library(shiny)
library(leaflet)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Childhood Asthma in School Zones"),
  
  
             h2("Severity of Symptoms Map"),
             leafletOutput("Middle_School_Severity"),
             h2("Presence of Environmental Hazards"),
             leafletOutput("Middle_School_Hazards")
  
)
