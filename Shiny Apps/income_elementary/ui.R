library(shiny)
library(leaflet)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Income and School Zones of Interest"),
  
  # Sidebar with a slider input for the number of bins
  
  sidebarLayout(
    sidebarPanel(
      p("Income data is from Simply Analytics taken from the U.S. Census in 2017."),
      p("School zone maps were taken from http://cohgis-mycity.opendata.arcgis.com/datasets/hisd-elementary-boundary")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("income_and_elementary_schools")
    )
  )
))