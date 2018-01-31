library(shiny)
library(leaflet)


HCAT_prime=readRDS("HCAT_prime.RDS")
HCAT_real=readRDS("HCAT_real.RDS")
options=intersect(colnames(HCAT_prime@data),colnames(HCAT_real@data))
options=options[8:length(options)]

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
      tabsetPanel(
        tabPanel("Plots",leafletOutput("HCATreal"),
            leafletOutput("HCATprime")),
        tabPanel("Table",tableOutput("Table"))
      )
      
    )
  )
))