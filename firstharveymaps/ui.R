library(shiny)
library(leaflet)

#floodplain data from:
#http://www.h-gac.com/rds/gis-data/gis-datasets.aspx

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Dan's Hurricane Harvey"),
  
  tabsetPanel(
    tabPanel("Children", leafletOutput("Children")), 
    tabPanel("Adults with Asthma", leafletOutput("AdultswuthAsthma")), 
    tabPanel("Children with Asthma", leafletOutput("ChildrenwithAsthma"))
  )
  )
)