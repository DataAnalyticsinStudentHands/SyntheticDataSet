library(shiny)
library(leaflet)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Projected Childhood Asthma in School Zones (This is Simulated)"),
  p("A synthetic data set was created using Census tract information to simulate the number of households and people with characteristics such as race, gender, educational attainment, and age. Health behvaiors and conditions were then simulated by matching with the CDC's Behavioral Risk Factor Surveillance system by sex, race, and income. Child Asthma was originally simulated in this way, further variables such as whether or not they had visited the ER for an asthma related emergency in the past 12 months or the presence of mold and cockroaches in the house were simulated by further matching with the Child Asthma Call Back survey. These simulated people were then randomly put into houses within a census tract using data from the Harris County Appraisal District, and the school the children were expected to go to based off of their simulated age and school zone available at http://cohgis-mycity.opendata.arcgis.com/datasets."),
  
  
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
