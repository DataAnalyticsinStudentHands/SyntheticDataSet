library(shiny)
library(leaflet)

realfrequencypertract=read.csv("../maps/realfrequenciespertract.csv")
syntheticfrequencypertract=read.csv("../maps/syntheticdatasetfrequenciespertract.csv")
options=intersect(colnames(realfrequencypertract),colnames(syntheticfrequencypertract))
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