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
      
      submitButton("Query Address"),
      
      helpText("Or upload csv files of addresses also in all capital letters and download results below"),
      
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      downloadButton('downloadData', 'Download')),
      #downloadButton("downloadData", "Download")),
    
      
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      textOutput("eligibility_status"),
      tableOutput("for_csv")
      
      
    )
  )
)
