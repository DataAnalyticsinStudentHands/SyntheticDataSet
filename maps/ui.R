library(shiny)
library(leaflet)

data=read.csv('syntheticdatasetfrequenciespertract.csv')
options=colnames(data)
options=options[3:length(options)]

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Hello Shiny!"),
  
  # Sidebar with a slider input for the number of bins
  
  sidebarLayout(
    sidebarPanel(
      selectInput("Variable",
                  "Variable of interest",
                  options)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("houstonreal"),
      leafletOutput("houstonprime")
    )
  )
))