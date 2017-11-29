library(shiny)
library(leaflet)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Projected Childhood Asthma in School Zones (This is Simulated)"),
  
  tabsetPanel(
    tabPanel("Elementary Schools",
             h2("Severity of Symptoms Map"),
             leafletOutput("Elementary_School_Severity"),
             h2("Presence of Environmental Hazards"),
             leafletOutput("Elementary_School_Hazards")),
    tabPanel("Middle Schools",
             h2("Severity of Symptoms Map"),
             leafletOutput("Middle_School_Severity"),
             h2("Presence of Environmental Hazards"),
             leafletOutput("Middle_School_Hazards")),
    tabPanel("High Schools",
             h2("Severity of Symptoms Map"),
             leafletOutput("High_School_Severity"),
             h2("Presence of Environmental Hazards"),
             leafletOutput("High_School_Hazards"))
  )
)
