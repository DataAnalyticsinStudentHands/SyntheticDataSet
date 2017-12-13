library(shiny)
library(leaflet)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Projected Childhood Asthma in School Zones (This is Simulated)"),
  
  tabsetPanel(
    tabPanel("Maps",
             fluidRow(
               column(4,
                      h4("Elementary Schools Severity of Symptoms Map"),
                      leafletOutput("Elementary_School_Severity"),
                      h4("Elementary Schools Presence of Environmental Hazards"),
                      leafletOutput("Elementary_School_Hazards")),
               column(4,
                      h4("Middle Schools Severity of Symptoms Map"),
                      leafletOutput("Middle_School_Severity"),
                      h4("Middle Schools Presence of Environmental Hazards"),
                      leafletOutput("Middle_School_Hazards")),
               column(4,
                      h4("High Schools Severity of Symptoms Map"),
                      leafletOutput("High_School_Severity"),
                      h4("High Schools Presence of Environmental Hazards"),
                      leafletOutput("High_School_Hazards"))
             )
            ),
    tabPanel("Elementary Schools Table",
             tableOutput("Elementary_School_Table")
    ),
    tabPanel("Middle Schools Table",
             tableOutput("Middle_School_Table")
             ),
    tabPanel("High Schools Table",
             tableOutput("High_School_Table"))
  )
)
