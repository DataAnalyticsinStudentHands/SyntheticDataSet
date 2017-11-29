# Define server logic required to draw a histogram ----

library(leaflet)

server <- function(input, output) {
  
  output$Middle_School_Severity<-renderLeaflet({
    Middle_School_Severity=readRDS("Middle_School_Severity_Map.rds")
  })
  
  output$Middle_School_Hazards<-renderLeaflet({
    Middle_School_Severity=readRDS("Middle_School_Hazards_Map.rds")
  })

   
}