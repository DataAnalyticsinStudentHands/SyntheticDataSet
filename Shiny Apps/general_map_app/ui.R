library(shiny)
library(leaflet)

#realfrequencypertract=read.csv("realfrequenciespertract.csv")
#syntheticfrequencypertract=read.csv("syntheticdatasetfrequenciespertract.csv")
#realfrequencypertract=readRDS("realfrequencypertract.RDS")
#syntheticfrequencypertract=readRDS("syntheticdatasetfrequencypertract.RDS")
#options=intersect(colnames(realfrequencypertract),colnames(syntheticfrequencypertract))
#options=options[3:length(options)]

Houston_prime=readRDS("Houston_prime.RDS")
Houston_real=readRDS("Houston_real.RDS")
options=intersect(colnames(Houston_prime@data),colnames(Houston_real@data))
options=options[9:135]

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Comparison of Real and Simulated Houston Data"),
  
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