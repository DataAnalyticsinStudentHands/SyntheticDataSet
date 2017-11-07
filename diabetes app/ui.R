library(shiny)


ui <- fluidPage(
  
  # App title ----
  titlePanel("Project Effects of a Diabetes Prevention Program around Houston"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      textInput("text", h3("Text input"), 
                value  = "14060 Dublin St"),
      
      submitButton("Query Address"),
      
      helpText("Or upload csv files of addresses")),
      
      #fileInput("file1", "Choose CSV File",
       #         accept = c(
        #          "text/csv",
         #         "text/comma-separated-values,text/plain",
          #        ".csv")
      #)),

    
      
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      textOutput("projected_particpants"),
      plotOutput("cumulative_cases"),
      plotOutput("cumulative_costs"),
      plotOutput("net_costs")
      
    )
  )
)
