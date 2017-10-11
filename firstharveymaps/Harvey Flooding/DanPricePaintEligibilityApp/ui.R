library(shiny)


ui <- fluidPage(
  
  # App title ----
  titlePanel("Check if Address is Eligible!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      textInput("text", h3("Text input"), 
                value  = "Enter Address in All Capital Letters"),
      
      submitButton("Query Address")),
    
      
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      textOutput("eligibility_status")
      
    )
  )
)
